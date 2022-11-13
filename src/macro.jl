macro ga(sig, ex)
  s = extract_signature(sig)
  ex = extract_base_expression(ex, s)
  ex = simplify(ex, s)
  to_expr(ex)
end

function extract_signature(ex::Expr)
  Meta.isexpr(ex, :tuple) || error("Expected tuple as signature, got $ex")
  all(isa(x, Int) for x in ex.args) || error("Expected literals in signature, got $ex")
  s = Signature(ex.args...)
end

const TRANSPOSE_SYMBOL = Symbol("'")

isreserved(op::Symbol) = in(op, (:∧, :∨, :⋅, :⦿, :*, :+, TRANSPOSE_SYMBOL))

function extract_grade(t::Symbol, ::S) where {S<:Signature}
  t === :Scalar && return 0
  t === :Vector && return 1
  t === :Bivector && return 2
  t === :Trivector && return 3
  t === :Quadvector && return 4
  t === :Pseudoscalar && return dimension(S)
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
      Expression(op, ex.args[2:end])
    elseif Meta.isexpr(ex, :(::))
      ex, T = ex.args
      g = extract_grade(T, s)
      if isa(ex, Expression)
        isnothing(g) && return ex
        !isa(g, Int) && isa(g, AbstractVector) && error("Multiple projections are not yet supported.")
        Expression(:project, g, ex)
      else
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
    else
      ex
    end
  end
end

extract_weights(::S, ex, g::Int, offset::Int) where {S<:Signature} = [:(getcomponent($ex, $(i + offset))) for i in 1:nelements(S, g)]

blade_expressions(::S, g::Int) where {S<:Signature} = [Expression(:blade, [Expression(:basis, i) for i in is]) for is in combinations(1:dimension(S), g)]

function kvector_expression(s::S, ex, g::Int, offset::Int = 0) where {S<:Signature}
  Expression(:kvector, [weighted(blade, w) for (blade, w) in zip(blade_expressions(s, g), extract_weights(s, ex, g, offset))])
end

function walk(ex::Expr, inner, outer)
  new_ex = Expr(ex.head)
  for arg in ex.args
    new = inner(arg)
    !isnothing(new) && push!(new_ex.args, new)
  end
  outer(new_ex)
end

function simplify(ex::Expression, s::Signature)
  # Flatten everything.
  ex = distribute(ex)

  # Structure the different parts into multivectors and k-vectors.
  ex = restructure_sums(ex)

  # Apply algebraic rules.
  ex = apply_projections(ex)
  ex = canonicalize_blades(ex)
  ex = apply_metric(ex, s)

  # Restructure the result.
  ex = disassociate_kvectors(ex)
  ex = group_kvector_blades(ex)
end

function to_expr(ex::Expression)
  # TODO: Convert to `Expr`.
  ex
end
