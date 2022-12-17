const DEFAULT_FLATTENING = :nested
const DEFAULT_TYPE = nothing

macro ga(sig_ex, flatten, T, ex)
  isa(flatten, QuoteNode) || error("Expected QuoteNode symbol for `flatten` argument, got ", repr(flatten))
  flatten = flatten.value
  esc(codegen_expression(sig_ex, ex; flatten, T))
end
macro ga(sig_ex, T_or_flatten, ex) isa(T_or_flatten, QuoteNode) ? esc(:(@ga $sig_ex $T_or_flatten $DEFAULT_TYPE $ex)) : esc(:(@ga $sig_ex $(QuoteNode(DEFAULT_FLATTENING)) $T_or_flatten $ex)) end
macro ga(sig_ex, ex) esc(:(@ga $sig_ex $nothing $ex)) end

struct VariableInfo
  refs::Dict{Symbol,Any}
  funcs::Dict{Symbol,Any}
  warn_override::Bool
  ref_sourcelocs::Dict{Symbol,LineNumberNode}
  func_sourcelocs::Dict{Symbol,LineNumberNode}
end

VariableInfo(; refs = Dict(), funcs = Dict(), warn_override::Bool = true) = VariableInfo(refs, funcs, warn_override, Dict(), Dict())

function Base.merge!(x::VariableInfo, y::VariableInfo)
  warn_override = x.warn_override & y.warn_override
  for (k, v) in pairs(y.refs)
    if haskey(x.refs, k) && warn_override
      ln = get(y.ref_sourcelocs, k, nothing)
      @warn "Redefinition of built-in variable `$k`$(sourceloc(ln))."
    end
    x.refs[k] = v
  end
  for (k, v) in pairs(y.funcs)
    if haskey(x.funcs, k) && warn_override
      ln = get(y.func_sourcelocs, k, nothing)
      @warn "Redefinition of built-in function `$k`$(sourceloc(ln)) (only one method is allowed)."
    end
    x.funcs[k] = v
  end
  x
end

macro arg(i)
  i > 0 || error("Argument slots must be positive integers.")
  QuoteNode(Expr(:argument, i))
end

function builtin_varinfo(sig::Signature; warn_override::Bool = true)
  refs = Dict{Symbol,Any}(
    :𝟏 => :(1::e),
    :𝟙 => :(1::$(Symbol(:e, join(1:dimension(sig))))),
    :⊣ => :left_interior_product,
    :⊢ => :right_interior_product,
    :⨼ => :left_interior_antiproduct,
    :⨽ => :right_interior_antiproduct,
    :⩒ => :geometric_antiproduct,
    :exterior_product => :∧,
    :∨ => :exterior_antiproduct,
    :geometric_product => :⟑,
    :○ => :interior_antiproduct,
    :⋅ => :●,
    :interior_product => :●,
    :⦿ => :scalar_product,
    :/ => :division,
    :division => :right_division,
    :antidivision => :right_antidivision,
  )

  funcs = Dict{Symbol,Any}(
    :bulk_left_complement => :(antireverse($(@arg 1)) ⟑ 𝟙),
    :bulk_right_complement => :(reverse($(@arg 1)) ⟑ 𝟙),
    :weight_left_complement => :(𝟏 ⩒ antireverse($(@arg 1))),
    :weight_right_complement => :(𝟏 ⩒ reverse($(@arg 1))),
    :bulk => :(weight_left_complement(bulk_right_complement($(@arg 1)))),
    :weight => :(bulk_left_complement(weight_right_complement($(@arg 1)))),

    :left_interior_product => :(left_complement($(@arg 1)) ∨ $(@arg 2)),
    :right_interior_product => :($(@arg 1) ∨ right_complement($(@arg 2))),
    :scalar_product => :(geometric_product($(@arg 1), reverse($(@arg 1)))::Scalar),
    :left_interior_antiproduct => :($(@arg 1) ∧ right_complement($(@arg 2))),
    :right_interior_antiproduct => :(left_complement($(@arg 1)) ∧ $(@arg 2)),

    :bulk_norm => :(sqrt(interior_product($(@arg 1), reverse($(@arg 1))))::e),
    :weight_norm => :(sqrt(interior_antiproduct($(@arg 1), antireverse($(@arg 1))))::e̅),
    :geometric_norm => :(bulk_norm($(@arg 1)) + weight_norm($(@arg 1))),
    :unitize => :(right_antidivision(weight($(@arg 1)), weight_norm($(@arg 1))) + bulk($(@arg 1))),

    :scalar_inverse => :(inv($(@arg 1))::Scalar),
    :inverse => :(reverse($(@arg 1)) * scalar_inverse(scalar_product($(@arg 1)))),
    :left_division => :(inverse($(@arg 1)) ⟑ $(@arg 2)),
    :right_division => :($(@arg 1) ⟑ inverse($(@arg 2))),

    :geometric_antiproduct => :(left_complement(geometric_product(right_complement($(@arg 1)), right_complement($(@arg 2))))),
    :exterior_antiproduct => :(left_complement(exterior_product(right_complement($(@arg 1)), right_complement($(@arg 2))))),
    :interior_antiproduct => :(left_complement(interior_product(right_complement($(@arg 1)), right_complement($(@arg 2))))),
    :antiscalar_product => :(geometric_antiproduct($(@arg 1), antireverse($(@arg 1)))::e̅),
    :antiinverse => :(left_complement(inverse(right_complement($(@arg 1))))),
    :left_antidivision => :(left_complement(left_division(right_complement($(@arg 1)), right_complement($(@arg 2))))),
    :right_antidivision => :(left_complement(right_division(right_complement($(@arg 1)), right_complement($(@arg 2))))),
  )

  VariableInfo(; refs, funcs, warn_override)
end

function generate_expression(sig::Signature, ex, varinfo::Optional{VariableInfo} = nothing)
  ex = extract_expression(ex, sig, varinfo)
  ex = restructure(ex, sig)
end

function codegen_expression(sig_ex, ex; flatten::Symbol = DEFAULT_FLATTENING, T = DEFAULT_TYPE, varinfo::Optional{VariableInfo} = nothing)
  in(flatten, (:flatten, :nested)) || error("Expected :flatten or :nested value for flattening keyword argument, got $flatten")
  flatten === :flatten && isnothing(T) && (T = :Tuple)
  sig = extract_signature(sig_ex)
  varinfo = merge!(builtin_varinfo(sig), @something(varinfo, VariableInfo()))
  to_final_expr(generate_expression(sig, ex, varinfo), sig, flatten == :flatten, T)
end

function extract_signature(ex)
  isa(ex, Integer) && return Signature(ex)
  isa(ex, Tuple) && return Signature(ex...)
  Meta.isexpr(ex, :tuple) || error("Expected tuple as signature, got $ex")
  all(isa(x, Int) for x in ex.args) || error("Expected literals in signature, got $ex")
  s = Signature(ex.args...)
end

const ADJOINT_SYMBOL = Symbol("'")

isreserved(op::Symbol) = in(op, (:⟑, :∧, :●, :*, :+, :×, :-, :reverse, :antireverse, :left_complement, :right_complement))

function extract_blade_from_annotation(t, sig::Signature)
  isa(t, Symbol) || return nothing
  (t === :e || t === :e0) && return blade()
  t === :e̅ && return antiscalar(sig)
  m = match(r"^e(\d+)$", string(t))
  dimension(sig) < 10 || error("A dimension of less than 10 is required to unambiguously refer to blades.")
  isnothing(m) && return nothing
  indices = parse.(Int, collect(m[1]))
  allunique(indices) || error("Index duplicates found in blade annotation $t.")
  maximum(indices) ≤ dimension(sig) || error("One or more indices exceeds the dimension of the specified space for blade $t")
  blade(indices)
end

# Examples:
# - extract grade 4 for expressions x::4, x::KVector{4}, x::Quadvector.
# - extract grades 1 and 3 for expressions x::(1 + 3), x::(Vector + Trivector), x::(KVector{1} + KVector{3}), x::Multivector{1, 3}, etc.
# We might want to reduce the number of syntactically valid expressions though.
function extract_grade_from_annotation(t, sig::Signature)
  isa(t, Int) && return t
  t === :Scalar && return 0
  t === :Vector && return 1
  t === :Bivector && return 2
  t === :Trivector && return 3
  t === :Quadvector && return 4
  t === :Antiscalar && return dimension(sig)
  t === :Multivector && return 0:dimension(sig)
  Meta.isexpr(t, :curly, 2) && t.args[1] === :KVector && return t.args[2]::Int
  Meta.isexpr(t, :annotate_projection) && t.args[1] === :+ && return Int[extract_grade_from_annotation(t, sig) for t in @view t.args[2:end]]
  Meta.isexpr(t, :curly) && t.args[1] === :Multivector && return @view t.args[2:end]
  error("Unknown grade projection for algebraic element $t")
end

function extract_expression(ex, sig::Signature, varinfo::VariableInfo)
  ex = expand_variables(ex, sig, varinfo)
  @debug "After variable expansion: $(stringc(ex))"

  # Make sure calls in annotations are not interpreted as actual operations.
  ex = prewalk(ex) do ex
    if Meta.isexpr(ex, :(::)) && Meta.isexpr(ex.args[2], :call)
      ex.args[2] = Expr(:annotate_projection, ex.args[2].args...)
    end
    ex
  end

  ex = postwalk(ex) do ex
    if Meta.isexpr(ex, ADJOINT_SYMBOL)
      simplified(sig, :reverse, ex.args[1]::Expression)
    elseif Meta.isexpr(ex, :call)
      op = ex.args[1]::Symbol
      !isreserved(op) && return ex
      args = ex.args[2:end]
      simplified(sig, op, args)
    elseif Meta.isexpr(ex, :(::))
      ex, T = ex.args
      b = extract_blade_from_annotation(T, sig)
      if !isnothing(b)
        !isa(ex, Expression) || error("Blade annotations for intermediate expressions are not supported.")
        return weighted(b, ex)
      end
      g = extract_grade_from_annotation(T, sig)
      isa(ex, Expression) && return project!(ex, g)
      input_expression(sig, ex, g)::Expression
    else
      ex
    end
  end

  isa(ex, Expression) || error("Could not fully extract expression: $ex\n\nOutermost expression has head $(ex.head) and arguments $(ex.args)\n")
  ex
end

sourceloc(ln::Nothing) = ""
sourceloc(ln::LineNumberNode) = string(" around ", ln.file, ':', ln.line)

function parse_variable_info(ex::Expr; warn_override::Bool = true)
  @assert Meta.isexpr(ex, :block)
  parse_variable_info(ex.args; warn_override)
end
function parse_variable_info(exs; warn_override::Bool = true)
  varinfo = VariableInfo(; warn_override)
  last_line = nothing
  for ex in exs
    if isa(ex, LineNumberNode)
      last_line = ex
      continue
    end

    if Meta.isexpr(ex, :(=)) && Meta.isexpr(ex.args[1], :call) || Meta.isexpr(ex, :function)
      # Extract function definition.
      call, body = Meta.isexpr(ex, :(=)) ? ex.args : ex.args
      Meta.isexpr(call, :where) && error("`where` clauses are not supported", sourceloc(last_line), '.')
      Meta.isexpr(body, :block, 2) && isa(body.args[1], LineNumberNode) && (body = body.args[2])
      name::Symbol, args... = call.args
      argtypes = extract_type.(args)
      all(isnothing, argtypes) || error("Function arguments must not have type annotations", sourceloc(last_line), '.')
      argnames = extract_name.(args)
      # Create slots so that we don't need to keep the names around; then we can directly place arguments by index.
      body = define_argument_slots(body, argnames)
      haskey(varinfo.funcs, name) && @warn "Redefinition of user-defined function `$name`$(sourceloc(last_line)) (only one method is allowed)."
      varinfo.funcs[name] = body
      !isnothing(last_line) && (varinfo.func_sourcelocs[name] = last_line)
      continue
    end

    if Meta.isexpr(ex, :(::)) && isa(ex.args[1], Symbol)
      # Type declaration.
      var, T = ex.args
      ex = :($var = $var::$T)
    end

    !Meta.isexpr(ex, :(=), 2) && error("Non-final expression parsed without a left-hand side assignment", sourceloc(last_line), '.')
    lhs, rhs = ex.args
    haskey(varinfo.refs, lhs) && @warn "Redefinition of user-defined variable `$lhs`$(sourceloc(last_line))."
    varinfo.refs[lhs] = rhs
    !isnothing(last_line) && (varinfo.ref_sourcelocs[lhs] = last_line)
  end
  varinfo
end

"""
Expand variables from a block expression, yielding a final expression where all variables were substitued with their defining expression.
"""
function expand_variables(ex, sig::Signature, varinfo::VariableInfo)
  !Meta.isexpr(ex, :block) && (ex = Expr(:block, ex))
  rhs = nothing

  i = findlast(x -> !isa(x, LineNumberNode), eachindex(ex.args))
  isnothing(i) && error("No input expression could be parsed.")
  parsed_varinfo = parse_variable_info(ex.args[1:(i - 1)])
  varinfo = merge!(deepcopy(varinfo), parsed_varinfo)

  last_ex = ex.args[i]
  rhs = Meta.isexpr(last_ex, :(=)) ? last_ex.args[2] : last_ex
  # Expand references and function calls.
  postwalk(ex -> expand_subtree(ex, varinfo.refs, varinfo.funcs, Set{Symbol}()), rhs)
end

function expand_subtree(ex, refs, funcs, used_refs)
  expanded_call = expand_function_call(funcs, ex)
  !isnothing(expanded_call) && return postwalk(x -> expand_subtree(x, refs, funcs, used_refs), expanded_call)
  !isa(ex, Symbol) && return ex
  used_refs_subtree = copy(used_refs)
  while isa(ex, Symbol) && !in(ex, used_refs_subtree)
    expanded_ref = expand_reference(refs, ex)
    isnothing(expanded_ref) && break
    ref, ex = expanded_ref
    push!(used_refs_subtree, ref)
    isa(ex, Expr) && return postwalk(x -> expand_subtree(x, refs, funcs, used_refs_subtree), ex)
  end
  ex
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
  fill_argument_slots(funcs[f], args, f)
end

function fill_argument_slots(ex, args, f::Symbol)
  postwalk(ex) do ex
    if Meta.isexpr(ex, :argument)
      arg = ex.args[1]::Int
      in(arg, eachindex(args)) || throw(ArgumentError("Not enough function arguments were provided to function '$f': expected $(argument_count(ex)) arguments, $(length(args)) were provided."))
      return args[arg]
    end
    ex
  end
end

function argument_count(ex)
  i = 0
  traverse(ex, Expr) do ex
    Meta.isexpr(ex, :argument) && (i = max(i, ex.args[1]::Int))
    nothing
  end
  i
end

function expand_reference(refs, ex)
  isa(ex, Symbol) || return nothing
  ref = get(refs, ex, nothing)
  isnothing(ref) && return nothing
  ex => ref
end

function extract_weights(sig::Signature, ex, g::Int, offset::Int)
  n = nelements(sig, g)
  n == 1 && return [:($getcomponent($ex))]
  [:($getcomponent($ex, $(i + offset))) for i in 1:n]
end

function input_expression(sig::Signature, ex, g::Int, offset::Int = 0)
  iszero(g) && return factor(:($getcomponent($ex)))
  simplified(sig, :+, Any[factor(w) * blade for (blade, w) in zip(map(blade, combinations(1:dimension(sig), g)), extract_weights(sig, ex, g, offset))])
end
function input_expression(sig::Signature, ex, g, offset::Int = 0)
  simplified(sig, :+, input_expression(sig, ex, g′, n′) for (g′, n′) in zip(g, cumsum(nelements(sig, g′) for g′ in g)))
end

function walk(ex::Expr, inner, outer)
  new_ex = Expr(ex.head)
  for arg in ex.args
    new = inner(arg)
    !isnothing(new) && push!(new_ex.args, new)
  end
  outer(new_ex)
end

function restructure(ex::Expression, sig::Signature)
  @debug "Before restructuring: $(stringc(ex))"

  # Structure the different parts into multivectors and k-vectors.
  # All `+` operations are replaced with k-vector or multivector expressions.
  ex = restructure_sums(ex, sig)
  @debug "After sum restructuring: $(stringc(ex))"

  # Add zero-factored components for blades not represented in k-vectors.
  ex = fill_kvector_components(ex, sig)
  @debug "After k-vector component filling: $(stringc(ex))"
  @debug "After restructuring: $(stringc(ex))"
  ex
end

function promote_to_expr(ex::Expression)
  isexpr(ex, :kvector) && return Some(Expr(:tuple, to_final_expr.(ex)))
  isexpr(ex, :blade) && return Some(nothing)
  isexpr(ex, :factor) && return Some(to_final_expr(ex[1]))
  nothing
end

function to_final_expr(arg, args...)
  final = to_expr(arg, args...)
  @assert !isnothing(final) "`nothing` value detected as output element for argument $arg"
  @assert !isa(final, Expression) "Expected non-Expression element, got $ex"
  final
end

reconstructed_type(T::Nothing, sig::Signature, ex) = :($KVector{$(ex.grade::Int), $(dimension(sig))})
reconstructed_type(T, sig::Signature, ex) = T

function to_expr(ex, sig::Optional{Signature} = nothing, flatten::Bool = false, T = nothing)
  if isexpr(ex, :multivector)
    if flatten
      @assert !isnothing(T)
      construction_exs = to_final_expr.(ex.args, sig::Signature, flatten, T)
      args = reduce(vcat, [subex.args[3].args for subex in construction_exs]; init = Any[])
      return :($construct($(reconstructed_type(T, sig::Signature, ex)), $(Expr(:tuple, args...))))
    else
      return Expr(:tuple, to_final_expr.(ex, sig::Signature, flatten, T)...)
    end
  end
  isexpr(ex, :factor) && return to_final_expr(ex[1])
  isexpr(ex, :kvector) && return :($construct($(reconstructed_type(T, sig::Signature, ex)), $(Expr(:tuple, to_final_expr.(ex.args)...))))
  isexpr(ex, :blade) && return 1
  if isexpr(ex, :⟑)
    @assert length(ex) == 2
    @assert isexpr(ex[2], :blade)
    return to_final_expr(ex[1]::Expression)
  end
  ex === Zero() && return 0
  # Follow through opaque function calls.
  Meta.isexpr(ex, :call) && return Expr(:call, ex.args[1], to_expr.(ex.args[2:end], sig, flatten, T)...)
  ex
end
