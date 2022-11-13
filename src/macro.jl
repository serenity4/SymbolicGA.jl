macro ga(sig, ex)
  s = extract_signature(sig)
  ex = extract_base_expression(ex, s)
  simplify(ex, s)
end

function extract_signature(ex::Expr)
  Meta.isexpr(ex, :tuple) || error("Expected tuple as signature, got $ex")
  all(isa(x, Int) for x in ex.args) || error("Expected literals in signature, got $ex")
  s = Signature(ex.args...)
end

const TRANSPOSE_SYMBOL = Symbol("'")

isreserved(op::Symbol) = in(op, (:∧, :∨, ⋅, ⦿, :*, :+, TRANSPOSE_SYMBOL))

function extract_grade(t::Symbol)
  t === :Scalar && return 0
  t === :Vector && return 1
  t === :Bivector && return 2
  t === :Trivector && return 3
  t === :Quadvector && return 4
  t === :Multivector && return nothing
  Meta.isexpr(t, :curly, 2) && t.args[1] === :KVector && return t.args[2]::Int
  Meta.isexpr(t, :curly) && t.args[1] === :Multivector && return @view t.args[2:end]
  error("Unknown grade projection for grade")
end

function extract_base_expression(ex::Expr, s::S) where {S<:Signature}
  postwalk(ex) do ex
    if Meta.isexpr(ex, :call)
      op = ex.args[1]::Symbol
      !isreserved(op) && return ex
      op == TRANSPOSE_SYMBOL && (op = :transpose)
      Expression(op, ex.args)
    elseif Meta.isexpr(ex, :(::))
      x, T = ex.args
      g = extract_grade(T)
      if isa(x, Expression)
        isnothing(g) && return x
        !isa(g, Int) && isa(g, AbstractVector) && error("Multiple projections are not yet supported.")
        Expression(:project, g, x)
      else
        g = extract_grade(T)
        if isa(g, Int)
          if iszero(g)
            Expression(:scalar, ex)
          else
            kvector_expression(s, ex, g)
          end
        else
          g = something(g, 0:dimension(S))
          Expression(:multivector, kvector_expression(s, ex, g′, n′) for (g′, n′) in zip(g, cumsum(nelements(S, g′) for g′ in g)))
        end
      end
    end
  end
end

extract_weights(::S, ex, g::Int, offset::Int) where {S<:Signature} = [:(getcomponent($ex, $(i + offset))) for i in 1:nelements(S, g)]

"""
    getcomponent(collection, i::Int)

Retrieve the `i`th component of a geometric element backed by `collection`, using a linear index in the range `1:cumsum(nelements(signature, g) for g in grades)`
where `signature` is the signature used for the algebra and `grades` the grades of the geometric element.
For example, a bivector in the geometric algebra over ℝ³ has three components, and therefore will use component indices from 1 to 3.

This falls back to `getindex(collection, i)`, so for most collections you won't need to extend this method.
"""
function getcomponent end

getcomponent(x, i) = getindex(x, i)

blade_expressions(::S, g::Int) where {S<:Signature} = [Expression(:blade, [Expression(:basis, i) for i in is]) for is in combinations(1:dimension(S), g)]

function kvector_expression(s::S, ex, g::Int, offset::Int = 0) where {S<:Signature}
  Expression(:kvector, [weighted(blade, w) for (blade, w) in zip(blade_expressions(s, g), extract_weights(s, ex, g, offset))])
end

walk(ex::Expr, inner, outer) = outer(Expr(ex.head, filter!(!isnothing, inner.(ex.args))))
