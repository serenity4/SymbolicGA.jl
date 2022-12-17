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

function distribute1(args, op::Symbol, sig::Optional{Signature})
  x, ys = args[1], @view args[2:end]
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
function restructure_sums(ex::Expression, sig::Signature)
  ex == factor(0) && return kvector(scalar(Zero()))
  if isblade(ex) || isweightedblade(ex)
    isone(nelements(sig, ex.grade::Int)) && return kvector(ex)
    terms = [ex]
  else
    @assert isexpr(ex, :+)
    terms = ex.args
  end

  # Fast path when all terms have the same grade.
  allequal(grade.(terms)) && return kvector(terms)

  args = sort(terms, by = grade)
  grades = grade.(args)
  i = 1
  new_args = []
  while i ≤ lastindex(grades)
    g = grades[i]
    j = findfirst(≠(g), @view grades[(i + 1):end])
    j = something(j, lastindex(grades) - (i - 1))
    j += i
    push!(new_args, kvector(args[i:(j - 1)]))
    i = j
  end
  multivector(new_args)
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
  orgex = ex
  prewalk(ex) do ex
    isexpr(ex, (:reverse, :antireverse)) || return ex
    reverse_op = ex.head
    anti = reverse_op === :antireverse
    anti && sig::Signature
    ex = ex[1]
    @assert isa(ex, Expression) "`Expression` argument expected for `$reverse_op`."
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
