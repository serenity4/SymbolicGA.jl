mutable struct RefinementMetrics
  reused::Int
  splits::Int
end

Base.show(io::IO, metrics::RefinementMetrics) = print(io, RefinementMetrics, '(', sprint(show, metrics), ")")
show_metrics(io::IO, metrics::RefinementMetrics) = print(io, metrics.reused, " reuses, ", metrics.splits, " splits")

struct IterativeRefinement
  # Having an vector of `ExpressionSpec` instead of `Expression`s
  # would allow lazy substitutions.
  # Lazy vs eager is in favor of lazy only if we intend to backtrack.
  # For now, we can directly mutate expressions.
  available::Dict{Int,Vector{Expression}}
  expressions::Vector{Expression}
  metrics::RefinementMetrics
end

function IterativeRefinement(exs::Vector{Expression})
  available = Dict{Int,Vector{Expression}}()
  for ex in exs
    make_available!(available, ex)
  end
  IterativeRefinement(available, exs, RefinementMetrics(0, 0))
end
IterativeRefinement(ex::Expression) = IterativeRefinement(gather_scalar_expressions!(Expression[], ex))

function make_available!(available::Dict{Int,Vector{Expression}}, ex::Expression)
  n = length(ex)
  available_n = get!(Vector{Expression}, available, n)
  !in(available_n, ex) && push!(available_n, ex)
end
make_available!(iter::IterativeRefinement, ex::Expression) = make_available!(iter.available, ex)

function may_reuse(ex::Expression, available::Expression)
  length(available) ≥ length(ex) && return false
  isexpr(ex, available.head) && all(in(ex), available.args)
end

function optimize!(ex::Expression)
  iter = IterativeRefinement(ex)
  apply!(iter)
  # @info "Optimization metrics: $(sprint(show_metrics, iter.metrics))"
  ex
end

function gather_scalar_expressions!(exs::Vector{Expression}, ex::Expression)
  (; cache) = ex
  traverse(ex; retraversal = Retraversal(cache, Expr)) do ex
    isexpr(ex, (SCALAR_ADDITION, SCALAR_PRODUCT)) && push!(exs, ex)
    true
  end
  exs
end

function apply!(iter::IterativeRefinement)
  exploited = exploit!(iter)
  @debug "Exploited: $exploited"
  explored = -1
  while exploited ≠ 0 || explored ≠ 0
    explored = explore!(iter)
    exploited = exploit!(iter)
    @debug "Exploited: $exploited, explored: $explored"
  end
end

function exploit!(iter::IterativeRefinement)
  exploited = 0
  while true
    exploited1 = exploit_available!(iter)
    exploited1 == 0 && return exploited
    exploited += exploited1
  end
  exploited
end

function exploit_available!(iter::IterativeRefinement)
  nreused = iter.metrics.reused
  # TODO: maybe to a 50/50 split instead of Sgoing for larger reuses first,
  # as it might yields more expressions that are split like 90/10 offering
  # a lesser potential for reuse.
  for (i, available) in sort(pairs(iter.available), by = first, rev = true)
    filter!(iter.expressions) do ex
      length(ex) == 2 && return false
      length(ex) ≤ i && return true
      reuses = findall(x -> may_reuse(ex, x), available)
      isempty(reuses) && return true
      reused, split = simulate_reuse(ex, available[first(reuses)])
      # TODO: use the split information to simulate what other reuses would be unlocked with the new split,
      # then choose the reuse with the greater potential among all possible reuses.
      reuse!(ex, reused, iter)
      length(ex) == 2
    end
  end
  iter.metrics.reused - nreused
end

function explore!(iter::IterativeRefinement)
  lengths = unique(length(ex) for ex in iter.expressions)
  nsplits = iter.metrics.splits
  for n in sort(lengths, rev = true)
    n ≤ 2 && return 0
    for ex in iter.expressions
      length(ex) == n || continue
      split!(ex, iter)
    end
    # Stop when at least 1 split has been performed.
    # This ensures that splits on large expressions can be
    # exploited before smaller expressions are split.
    explored = iter.metrics.splits - nsplits
    explored > 0 && return explored
  end
  0
end

function simulate_reuse(ex::Expression, reused::Expression)
  remaining = copy(reused.args)
  reused, split = Term[], Term[]
  for arg in ex
    if in(arg, remaining)
      push!(reused, arg)
      deleteat!(remaining, findfirst(x -> x === arg, remaining)::Int)
    else
      push!(split, arg)
    end
  end
  reused, split
end

function reuse!(ex::Expression, reused::Vector{Term})
  remaining = copy(reused)
  filter!(ex.args) do arg
    j = findfirst(x -> arg === x, remaining)
    isnothing(j) && return true
    deleteat!(remaining, j)
    false
  end
  @assert isempty(remaining)
  push!(ex.args, uncached_expression(ex.cache, ex.head, reused))
  @assert length(ex) > 1
  ex
end

function reuse!(ex::Expression, reused::Vector{Term}, iter::IterativeRefinement)
  n = length(ex)
  reuse!(ex, reused)
  available_n = iter.available[n]
  deleteat!(available_n, findfirst(x -> x === ex, available_n)::Int)
  make_available!(iter, ex)
  iter.metrics.reused += 1
  ex
end

function split!(ex::Expression, iter::IterativeRefinement)
  # Split 50/50.
  split = collect(Iterators.drop(ex.args, fld(length(ex.args), 2)))
  iter.metrics.splits += 1
  reuse!(ex, split, iter)
  # Don't record the reuse.
  iter.metrics.reused -= 1
end
