function disassociate1(args, op::Symbol, sig::Optional{Signature})
  new_args = []
  for arg in args
    if isexpr(arg, op)
      append!(new_args, arg.args)
    else
      push!(new_args, arg)
    end
  end
  simplified(sig, op, new_args)
end

function distribute1(ex::Expression, op::Symbol, sig::Optional{Signature})
  x, ys = ex[1], @view ex[2:end]
  base = isexpr(x, :+) ? x.args : [x]
  for y in ys
    new_base = []
    yterms = isexpr(y, :+) ? y.args : (y,)
    for xterm in base
      for yterm in yterms
        push!(new_base, simplified(sig, op, xterm, yterm))
      end
    end
    base = new_base
  end
  simplified(sig, :+, base)
end

"""
Convert sums between elements of arbitrary grades into either a k-vector or a multivector.

If all elements in the sum had the same grade, a k-vector is returned.
Otherwise, all elements of the same grade are grouped, wrapped in k-vectors
and added to a multivector expression.
"""
function restructure_sums(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, :+) || return ex

    if count(==(grade(ex[1]::Expression)), grade(arg::Expression) for arg in ex) == length(ex)
      return kvector(ex.args)
    end

    args = sort(ex.args, by = grade)
    grades = grade.(args)
    i = 1
    new_args = []
    while i ≤ lastindex(grades)
      g = grades[i]
      j = findfirst(≠(g), @view grades[(i + 1):end])
      j = something(j, lastindex(grades) - (i - 1))
      j += i
      if j > i + 1
        push!(new_args, iszero(g) ? +(args[i:(j - 1)]...) : kvector(args[i:(j - 1)]))
      else
        push!(new_args, args[i])
      end
      i = j
    end
    multivector(new_args)
  end
end

function project!(ex, g, level = 0)
  isa(ex, Expression) || return ex
  if isexpr(ex, :scalar)
    in(0, g) && return ex
    iszero(level) && @debug "Scalar expression annihilated in projection into grade(s) $g" ex
    return scalar(0)
  end
  if all(isempty(intersect(g, g′)) for g′ in grade(ex))
    iszero(level) && @debug "Non-scalar expression annihilated in projection into grade(s) $g" ex
    return scalar(0)
  end
  if isexpr(ex, :+)
    for (i, x) in enumerate(ex)
      ex.args[i] = project!(x, g, level + 1)
    end
    return simplify!(ex)
  end
  ex
end

function apply_reverse_operators(ex::Expression)
  prewalk(ex) do ex
    isexpr(ex, :reverse) || return ex
    arg = ex[1]
    isa(arg, Expression) || error("Expected an expression as argument to `reverse`.")
    if isexpr(arg, (:+, :kvector, :multivector)) # distribute over addition
      Expression(arg.head, propagate_reverse(arg.args))
    elseif isexpr(arg, (:*, :blade))
      (; args) = arg
      new_args = isexpr(arg, :blade) || count(x -> x.grade !== 0, args) > 1 ? reverse(args) : args
      !isexpr(arg, (:scalar, :blade)) && (new_args = propagate_reverse(new_args))
      Expression(arg.head, new_args)
    else
      @assert false "Unexpected operator $(arg.head) encountered when applying reverse operators."
    end
  end
end

function propagate_reverse(args)
  res = Any[]
  for arg in args
    arg::Expression
    in(arg.grade, (0, 1)) ? push!(res, arg) : push!(res, reverse(arg))
  end
  res
end

function group_kvector_blades(ex::Expression)
  isexpr(ex, :multivector) && return Expression(:multivector, group_kvector_blades.(ex.args))
  isexpr(ex, :scalar) && return ex
  isexpr(ex, :blade) && return ex
  isweighted(ex) && isexpr(ex[2], :blade) && return ex
  @assert isexpr(ex, :kvector) "Expected k-vector expression, got $ex"

  # Fast path for 0-vectors.
  grade(ex) == 0 && return kvector(Expression(:+, ex.args))

  blade_weights = Dict{Vector{Int},Expression}()
  for arg in ex.args
    if isweighted(arg)
      weight, blade = arg[1]::Expression, arg[2]::Expression
    else
      weight, blade = Expression(:scalar, 1), arg
    end
    indices = basis_vectors(arg)
    if haskey(blade_weights, indices)
      blade_weights[indices] = blade_weights[indices] + weight
    else
      blade_weights[indices] = weight
    end
  end
  kvector(Any[weight * blade(indices) for (indices, weight) in pairs(blade_weights)])
end

function fill_kvector_components(ex::Expression, s::Signature)
  postwalk(ex) do ex
    isexpr(ex, :kvector) || return ex
    g = grade(ex)
    i = 1
    ex = kvector(sort(ex.args, by = basis_vectors, lt = lt_basis_order))
    for indices in combinations(1:dimension(s), g)
      next = i ≤ lastindex(ex) ? ex[i]::Expression : nothing
      if isnothing(next) || indices ≠ basis_vectors(next)
        insert!(ex.args, i, scalar(0))
      end
      i += 1
    end
    ex
  end
end
