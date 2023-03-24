mutable struct RefinementMetrics
  reused::Int
end

Base.show(io::IO, metrics::RefinementMetrics) = print(io, RefinementMetrics, '(', metrics.reused, " reuses)")

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
  IterativeRefinement(available, exs, RefinementMetrics(0))
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
  # TODO: maybe to a 50/50 split instead of going for larger reuses first,
  # as it might yields more expressions that are split like 90/10 offering
  # a lesser potential for reuse.
  for (i, available) in sort(pairs(iter.available), by = first, rev = true)
    filter!(iter.expressions) do ex
      length(ex) == 2 && return false
      length(ex) ≤ i && return true
      reuses = findall(Base.Fix1(may_reuse, ex), available)
      isempty(reuses) && return true
      reused, split = simulate_reuse(ex, available[first(reuses)])
      # TODO: use the split information to simulate what other reuses would be unlocked with the new split,
      # then choose the reuse with the greater potential among all possible reuses.
      reuse!(ex, reused, iter)
      length(ex) == 2
    end
  end
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
