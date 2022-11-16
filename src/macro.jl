macro ga(T, sig, ex)
  s = extract_signature(sig)
  ex = extract_base_expression(ex, s)
  ex = simplify(ex, s)
  ex = to_expr(ex)
  esc(:($construct($T, $ex)))
end

macro ga(sig, ex)
  esc(:(@ga Tuple $sig $ex))
end

function extract_signature(ex)
  isa(ex, Integer) && return Signature(ex)
  Meta.isexpr(ex, :tuple) || error("Expected tuple as signature, got $ex")
  all(isa(x, Int) for x in ex.args) || error("Expected literals in signature, got $ex")
  s = Signature(ex.args...)
end

const TRANSPOSE_SYMBOL = Symbol("'")

isreserved(op::Symbol) = in(op, (:∧, :∨, :⋅, :⦿, :*, :+, :×, :-, TRANSPOSE_SYMBOL))

function extract_blade(t::Symbol, ::S) where {S<:Signature}
  (t === :e || t === :e0) && return :scalar
  m = match(r"^e(\d+)$", string(t))
  dimension(S) < 10 || error("A dimension of less than 10 is required to unambiguously refer to blades.")
  isnothing(m) && return nothing
  indices = parse.(Int, collect(m[1]))
  allunique(indices) || error("Index duplicates found in blade annotation $t.")
  maximum(indices) ≤ dimension(S) || error("One or more indices exceeds the dimension of the specified space for blade $t")
  blade(indices)
end

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
  Meta.isexpr(ex, :block) && (ex = expand_variables(ex))
  postwalk(ex) do ex
    if Meta.isexpr(ex, :call)
      op = ex.args[1]::Symbol
      !isreserved(op) && return ex
      op == TRANSPOSE_SYMBOL && (op = :transpose)
      Expression(op, ex.args[2:end])
    elseif Meta.isexpr(ex, :(::))
      ex, T = ex.args
      b = extract_blade(T, s)
      b === :scalar && return scalar(ex)
      !isnothing(b) && return scalar(ex) * b
      g = extract_grade(T, s)
      if isa(ex, Expression)
        isnothing(g) && return ex
        !isa(g, Int) && isa(g, AbstractVector) && error("Multiple projections are not yet supported.")
        project(g, ex)
      else
        if isa(g, Int)
          if iszero(g)
            Expression(:scalar, :($getcomponent($ex)))
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

"""
Expand variables from a block expression, yielding a final expression where all variables were substitued with their defining expression.
"""
function expand_variables(ex::Expr)
  @assert Meta.isexpr(ex, :block)
  refs = Dict{Symbol,Any}()
  rhs = nothing
  for subex in ex.args
    isa(subex, LineNumberNode) && continue
    lhs, rhs = Meta.isexpr(subex, :(=), 2) ? subex.args : (nothing, subex)
    # Expand known variables.
    rhs = postwalk(rhs) do x
      isa(x, Symbol) || return x
      get(refs, x, x)
    end
    !isnothing(lhs) && (refs[lhs] = rhs)
  end
  rhs
end

function extract_weights(::S, ex, g::Int, offset::Int) where {S<:Signature}
  n = nelements(S, g)
  n == 1 && return [:($getcomponent($ex))]
  [:($getcomponent($ex, $(i + offset))) for i in 1:n]
end

function kvector_expression(s::S, ex, g::Int, offset::Int = 0) where {S<:Signature}
  kvector(Any[weighted(blade, w) for (blade, w) in zip(map(blade, combinations(1:dimension(S), g)), extract_weights(s, ex, g, offset))])
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
  @debug "Base expression: $(stringc(ex))"
  ex = expand_operators(ex)
  @debug "After operator expansion: $(stringc(ex))"

  # Flatten everything.
  ex = distribute(ex)
  @debug "After distribution: $(stringc(ex))"

  # Structure the different parts into multivectors and k-vectors.
  ex = restructure_sums(ex)
  @debug "After sum restructuring: $(stringc(ex))"

  # Apply algebraic rules.
  ex = apply_projections(ex)
  @debug "After projection filtering: $(stringc(ex))"
  ex = canonicalize_blades(ex)
  @debug "After blade canonicalization: $(stringc(ex))"
  ex = apply_metric(ex, s)
  @debug "After metric simplifications: $(stringc(ex))"

  # Restructure the result.
  ex = disassociate_kvectors(ex)
  @debug "After k-vector disassocation: $(stringc(ex))"
  ex = group_kvector_blades(ex)
  @debug "After blade grouping: $(stringc(ex))"
  ex = fill_kvector_components(ex, s)
  @debug "After k-vector component filling: $(stringc(ex))"
  @debug "After all transforms: $(stringc(ex))"
  ex
end

function promote_to_expr(ex::Expression)
  isexpr(ex, :kvector) && return Some(Expr(:tuple, ex.args...))
  isexpr(ex, :blade) && return Some(nothing)
  isexpr(ex, :scalar) && return Some(ex[1])
  nothing
end

function to_expr(ex)
  if isexpr(ex, :multivector)
    expr = Expr(:tuple)
    for arg in ex.args
      arg = to_expr(arg)
      if Meta.isexpr(arg, :tuple)
        append!(expr.args, arg.args)
      else
        push!(expr.args, arg)
      end
    end
    return expr
  end
  isexpr(ex, :scalar) && return to_expr(ex[1])
  isexpr(ex, :kvector) && return length(ex) == 1 ? to_expr(only(ex)) : Expr(:tuple, to_expr.(ex.args)...)
  isexpr(ex, :blade) && return nothing
  if isexpr(ex, :*)
    @assert length(ex) == 2
    @assert isexpr(ex[2], :blade)
    return to_expr(ex[1])
  end
  @assert !isa(ex, Expression) "Expected non-Expression element, got $ex"
  ex
end
