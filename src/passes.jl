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
        push!(new_args, kvector(args[i:(j - 1)]))
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
  if all(isempty(intersect(g, g′)) for g′ in grade(ex))
    iszero(level) && @debug "Non-scalar expression annihilated in projection into grade(s) $g" ex
    return factor(0)
  end
  if isexpr(ex, :+)
    for (i, x) in enumerate(ex)
      ex.args[i] = project!(x, g, level + 1)
    end
    return simplify!(ex)
  end
  ex
end

function simplify_negations(fac)
  if Meta.isexpr(fac, :call) && fac.args[1] === :*
    n = count(x -> x === -1, fac.args)
    if n > 1
      sc_args = filter(x -> x !== -1, fac.args)
      isodd(n) && insert!(sc_args, 2, -1)
      length(sc_args) == 1 && return 1
      return Expr(:call, sc_args...)
    end
  end
  fac
end

function apply_reverse_operators(ex::Expression, sig::Optional{Signature})
  prewalk(ex) do ex
    isexpr(ex, (:reverse, :antireverse)) || return ex
    reverse_op = ex.head
    anti = reverse_op === :antireverse
    anti && sig::Signature
    ex = ex[1]
    isa(ex, Expression) || error("Expected an expression as argument to `$reverse_op`.")
    # Distribute over addition.
    isexpr(ex, (:+, :kvector, :multivector)) && return simplified(sig, ex.head, anti ? antireverse.(sig, ex) : reverse.(ex))
    !anti && in(ex.grade, (0, 1)) && return ex
    anti && in(antigrade(sig, ex.grade), (0, 1)) && return ex
    isexpr(ex, :⟑) && return propagate_reverse(reverse_op, ex, sig)
    @assert isexpr(ex, :blade) "Unexpected operator $head encountered when applying $reverse_op operators."
    n = anti ? antigrade(sig, ex)::Int : grade(ex)::Int
    fac = (-1)^(n * (n - 1) ÷ 2)
    isone(fac) ? ex : -ex
  end
end

# grade(x) + antigrade(x) == dimension(sig)
antigrade(sig::Signature, g::Int) = dimension(sig) - g
antigrade(sig::Signature, g::Vector{Int}) = antigrade.(sig, g)
antigrade(sig::Signature, ex::Expression) = antigrade(sig, ex.grade)

function propagate_reverse(reverse_op, ex::Expression, sig::Optional{Signature})
  res = Any[]
  for arg in ex
    arg::Expression
    in(arg.grade, (0, 1)) ? push!(res, arg) : push!(res, simplified(sig, reverse_op, arg))
  end
  simplified(sig, ex.head, res)
end

function group_kvector_blades(ex::Expression)
  isexpr(ex, :multivector) && return Expression(:multivector, group_kvector_blades.(ex.args))
  isexpr(ex, :blade) && return ex
  isweightedblade(ex) && return ex
  @assert isexpr(ex, :kvector) "Expected k-vector expression, got $ex"

  blade_weights = Dict{Vector{Int},Expression}()
  for arg in ex.args
    if isweighted(arg)
      weight, blade = arg[1]::Expression, arg[2]::Expression
    else
      weight, blade = factor(1), arg
    end
    indices = basis_vectors(arg)
    if haskey(blade_weights, indices)
      blade_weights[indices] = blade_weights[indices] + weight
    else
      blade_weights[indices] = weight
    end
  end
  kvector(Any[(weight == factor(0) ? factor(Zero()) : weight) * blade(indices) for (indices, weight) in pairs(blade_weights)])
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
        insert!(ex.args, i, weighted(blade(indices), Zero()))
      end
      i += 1
    end
    ex
  end
end

"Non-simplifiable zero, for use in factors."
struct Zero end
Base.show(io::IO, ::Zero) = print(io, '𝟎')
