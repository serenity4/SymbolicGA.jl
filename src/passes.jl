function distribute(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, (:kvector, :multivector)) && (ex = Expression(:+, ex.args))
    isexpr(ex, :+) && return disassociate1(ex, :+)
    isexpr(ex, :*) && any(isexpr(arg, :+) for arg in ex.args) && return distribute1(ex)
    ex
  end
end

function disassociate1(args, op::Symbol)
  new_args = []
  for arg in args
    if isexpr(arg, op)
      append!(new_args, arg.args)
    else
      push!(new_args, arg)
    end
  end
  new_args
end

disassociate1(ex::Expression, op::Symbol) = Expression(op, disassociate1(ex.args, op))

function distribute1(ex::Expression)
  new_args = []
  for (i, arg) in enumerate(ex.args)
    if isexpr(arg, :+)
      for x in arg.args
        for arg2 in @view ex[(i + 1):end]
          if isexpr(arg2, :+)
            for y in arg2.args
              push!(new_args, Expression(:*, x, y))
            end
          else
            push!(new_args, Expression(:*, x, arg2))
          end
        end
      end
    else
      push!(new_args, arg)
    end
  end
  Expression(:+, new_args)
end

function restructure_sums(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, :+) || return ex

    if count(==(ex[1].grade), arg.grade for arg in ex) == length(ex)
      return Expression(:kvector, ex.args)
    end

    args = sort(ex.args, by = x -> x.grade)
    grades = getproperty.(args, :grade)
    i = 1
    new_args = []
    while i ≤ lastindex(grades)
      g = grades[i]
      j = findfirst(≠(g), @view grades[(i + 1):end])
      j = something(j, lastindex(grades) - (i - 1))
      j += i
      if j > i + 1
        push!(new_args, Expression(:kvector, @view args[i:(j - 1)]))
      else
        push!(new_args, args[i])
      end
      i = j
    end
    Expression(:multivector, new_args)
  end
end

function apply_projections(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, :project) || return ex
    g, ex = ex.args
    if isexpr(ex, :multivector)
      Expression(:kvector, filter(x -> grade(x)::Int == g, ex.args))
    else
      Expression(ex.head, filter(x -> grade(x)::Int == g, ex.args))
    end
  end
end

function canonicalize_blades(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, :blade) || return ex
    perm = sortperm(ex.args, by = x -> x[1]::Int)
    new_ex = Expression(:blade, ex[perm])
    iseven(parity(perm)) && return new_ex
    Expression(:*, Expression(:scalar, -1), new_ex)
  end
end

function apply_metric(ex::Expression, s::Signature)
  postwalk(ex) do ex
    isexpr(ex, :blade) || return ex
    @assert all(isexpr(arg, :basis) for arg in ex.args)
    new_args = copy(ex.args)
    last = nothing
    fac = 1
    i = 1
    while length(new_args) ≥ 2
      i > lastindex(new_args) && break
      arg = new_args[i]
      new = arg[1]::Int
      if !isnothing(last)
        if new == last
          m = metric(s, last)
          iszero(m) && return nothing
          fac *= m
          deleteat!(new_args, i)
          deleteat!(new_args, i - 1)
          i = max(i - 2, 1)
          last = i < firstindex(new_args) ? nothing : new_args[i][1]::Int
        else
          last = new
          i += 1
        end
      else
        last = new
        i += 1
      end
    end
    if isempty(new_args)
      Expression(:scalar, fac)
    else
      new_ex = Expression(:blade, new_args)
      isone(fac) ? new_ex : Expression(:*, Expression(:scalar, fac), new_ex)
    end
  end
end

function disassociate_kvectors(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, :kvector) && any(isexpr(:kvector), ex.args) && return disassociate1(ex, :kvector)
    ex
  end
end

function group_kvector_blades(ex::Expression)
  blade_weights = Dict{Vector{Any},Expression}()
  for arg in ex.args
    if isweighted(arg)
      weight, blade = arg[1]::Expression, arg[2]::Expression
    else
      weight, blade = Expression(:scalar, 1), arg
    end
    indices = getindex.(blade.args, 1)
    if haskey(blade_weights, indices)
      blade_weights[indices] = Expression(:+, blade_weights[indices], weight)
    else
      blade_weights[indices] = weight
    end
  end
  Expression(:kvector, [weighted(Expression(:blade, Any[Expression(:basis, i) for i in indices]), weight) for (indices, weight) in pairs(blade_weights)])
end
