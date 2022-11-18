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
  error("Unknown grade projection for algebraic element $t")
end

function extract_base_expression(ex::Expr, s::S) where {S<:Signature}
  Meta.isexpr(ex, :block) && (ex = expand_variables(ex))
  @debug "After variable expansion: $(stringc(ex))"
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
  funcs = Dict{Symbol,Any}()
  rhs = nothing
  for subex in ex.args
    isa(subex, LineNumberNode) && continue

    if Meta.isexpr(subex, :(=)) && Meta.isexpr(subex.args[1], :call) || Meta.isexpr(subex, :function)
      # Extract function definition.
      call, body = Meta.isexpr(subex, :(=)) ? subex.args : subex.args
      Meta.isexpr(call, :where) && error("`where` clauses are not supported.")
      Meta.isexpr(body, :block, 2) && isa(body.args[1], LineNumberNode) && (body = body.args[2])
      name::Symbol, args... = call.args
      argtypes = extract_type.(args)
      all(isnothing, argtypes) || error("Function arguments cannot have type annotations.")
      argnames = extract_name.(args)
      # Create slots so that we don't need to keep the names around; then we can directly place arguments by index.
      body = define_argument_slots(body, argnames)
      if haskey(funcs, name)
        @warn "Replacing geometric algebra function `$name` (only one method is allowed)"
      end
      funcs[name] = body
      continue
    end

    if Meta.isexpr(subex, :(::)) && isa(subex.args[1], Symbol)
      # Type declaration.
      var, T = subex.args
      subex = :($var = $var::$T)
    end

    lhs, rhs = Meta.isexpr(subex, :(=), 2) ? subex.args : (nothing, subex)

    # Expand references and function calls.
    rhs = postwalk(rhs) do x
      expanded_call = expand_function_call(funcs, x)
      x = something(expanded_call, x)
      expanded_ref = expand_reference(refs, x)
      isnothing(expanded_ref) && isnothing(expanded_call) && return x
      used_refs = Set(Symbol[])
      if !isnothing(expanded_ref)
        ref, x = expanded_ref
        push!(used_refs, ref)
      end
      prewalk(x) do y
        expanded_call = expand_function_call(funcs, y)
        !isnothing(expanded_call) && return expanded_call
        isa(y, Symbol) || return y
        in(y, used_refs) && return y
        expanded_ref = expand_reference(refs, y)
        isnothing(expanded_ref) && return y
        ref, y = expanded_ref
        push!(used_refs, ref)
        y
      end
    end
    !isnothing(lhs) && (refs[lhs] = rhs)
  end
  rhs
end

extract_type(ex) = Meta.isexpr(ex, :(::), 2) ? ex.args[2] : nothing
extract_name(ex) = Meta.isexpr(ex, :(::), 2) ? ex.args[1] : isa(ex, Symbol) ? ex : error("Expected argument name to be a symbol, got $ex")

function define_argument_slots(ex, argnames)
  postwalk(ex) do ex
    if isa(ex, Symbol)
      i = findfirst(==(ex), argnames)
      !isnothing(i) && return Expr(:argument, i)
    end
    ex
  end
end

function expand_function_call(funcs, ex)
  !Meta.isexpr(ex, :call) && return nothing
  f::Symbol, args... = ex.args
  func = get(funcs, f, nothing)
  isnothing(func) && return nothing
  fill_argument_slots(funcs[f], ex, args)
end

function fill_argument_slots(ex, original_ex, args)
  argtypes = extract_type.(args)
  # all(!isnothing, argtypes) || error("Arguments must be type-annotated for function call $original_ex")
  postwalk(ex) do ex
    Meta.isexpr(ex, :argument) && return args[ex.args[1]::Int]
    ex
  end
end

function expand_reference(refs, ex)
  isa(ex, Symbol) || return nothing
  ref = get(refs, ex, nothing)
  isnothing(ref) && return nothing
  ex => ref
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
  isexpr(ex, :kvector) && return Some(Expr(:tuple, to_expr_final.(ex)))
  isexpr(ex, :blade) && return Some(nothing)
  isexpr(ex, :scalar) && return Some(to_expr(ex[1]))
  nothing
end

function to_expr_final(arg)
  final = to_expr(arg)
  @assert !isnothing(final) "`nothing` value detected as output element for argument $arg"
  final
end

function to_expr(ex)
  if isexpr(ex, :multivector)
    expr = Expr(:tuple)
    for arg in ex.args
      arg = to_expr_final(arg)
      if Meta.isexpr(arg, :tuple)
        append!(expr.args, arg.args)
      else
        push!(expr.args, arg)
      end
    end
    return expr
  end
  isexpr(ex, :scalar) && return to_expr(ex[1])
  isexpr(ex, :kvector) && return length(ex) == 1 ? to_expr(only(ex)) : Expr(:tuple, to_expr_final.(ex.args)...)
  isexpr(ex, :blade) && return 1
  if isexpr(ex, :*)
    @assert length(ex) == 2
    @assert isexpr(ex[2], :blade)
    return to_expr(ex[1])
  end
  @assert !isa(ex, Expression) "Expected non-Expression element, got $ex"
  ex
end
