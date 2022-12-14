const DEFAULT_FLATTENING = :nested
const DEFAULT_TYPE = nothing

"""
    @ga <sig> <flattening> <T> <ex>
    @ga <sig> <flattening or T> <ex>
    @ga <sig> <ex>

Generate Julia code which implements the computation of geometric elements from `ex` in an algebra defined by a signature `sig` (see [`SymbolicGA.Signature`](@ref)).

Supported syntax:
- `sig`: Integer literal or tuple of 1, 2 or 3 integer literals corresponding to the number of positive, negative and degenerate basis vectors respectively, where unspecified integers default to zero.
- `flattening`: Symbol literal.
- `T`: Any arbitrary expression which evaluates to a type or to `nothing`.
- `ex`: Any arbitrary expression that can be parsed algebraically.

See also: [`codegen_expression`](@ref).

`ex` can be a single statement or a block, and uses a domain-specific language to facilitate the construction of algebraic expressions.
`ex` is logically divided into two sections: a definition section, which defines bindings, and a final algebraic expression, which will be the object of the evaluation. It is processed in three phases:
- A definition phase, in which bindings are defined with one or several statements for use in the subsequent phase;
- An expansion phase, where identified bindings in the final algebraic expression are expanded. The defined bindings include the ones priorly defined and a set of built-in bindings.
- An evaluation phase, in which the core algebraic expression is simplified and translated into a Julia expression.

# Expression parsing

## Binding definitions

All statements prior to the last can define new variables or functions with the following syntax and semantics:
- Variables are either declared with `<lhs::Symbol> = <rhs::Any>` or with `<lhs::Symbol>::<type>`, the latter being expanded to `<lhs> = <lhs>::<type>`.
- Functions are declared with a standard short or long form function definition `<name>(<args...>) = <rhs>` or `function <name>(<args...>) <rhs> end`, and are restricted to simple forms to encode simple semantics. The restrictions are as follows:
  - `where` clauses and output type annotations are not supported.
  - Function arguments must be untyped, e.g. `f(x, y)` is allowed but not `f(x::Vector, y::Vector)`.
  - Function arguments must not be reassigned; it is assumed that any occurence of symbols declared as arguments will reference these arguments. For example, `f(x, y) = x + y` assumes that `x + y` actually means "perform `+` on the first and second function argument". Therefore, `f(x, y) = (x = 2; x) + y` will be likely to cause bugs. To alleviate this restriction, use [`codegen_expression`](@ref) with a suitable [`SymbolicGA.VariableInfo`] with function entries that contain specific calls to `:(\$(@arg <i>)).

## Binding expansion

References and functions are expanded in a fairly straightforward copy-paste manner, where references are replaced with their right-hand side and function calls with their bodies with their arguments interpolated. Simple checks are put in place to allow for self-referencing bindings for references, such as `x = x::T`, leading to a single expansion of such a pattern in the corresponding expression subtree.

See [`SymbolicGA.VariableInfo`](@ref) for more information regarding the expansion of such variables and functions.

# Algebraic evaluation

Type annotations may either:
- Specify what type of geometric entity an input should be considered as, where components are then picked off with `getcomponent`.
- Request the projection of an intermediate expression over one or multiple grades.
"""
macro ga end

propagate_source(__source__, ex) = Expr(:block, LineNumberNode(__source__.line, __source__.file), ex)

macro ga(sig_ex, flattening, T, ex)
  isa(flattening, QuoteNode) || error("Expected QuoteNode symbol for `flattening` argument, got ", repr(flattening))
  flattening = flattening.value
  ex = codegen_expression(sig_ex, ex; flattening, T)
  propagate_source(__source__, esc(ex))
end
macro ga(sig_ex, T_or_flattening, ex)
  ex = if isa(T_or_flattening, QuoteNode)
    :(@ga $sig_ex $T_or_flattening $DEFAULT_TYPE $ex)
  else
    :(@ga $sig_ex $(QuoteNode(DEFAULT_FLATTENING)) $T_or_flattening $ex)
  end
  propagate_source(__source__, esc(ex))
end
macro ga(sig_ex, ex)
  ex = :(@ga $sig_ex $nothing $ex)
  propagate_source(__source__, esc(ex))
end

"""
    VariableInfo(; refs = Dict{Symbol,Any}(), funcs = Dict{Symbol,Any}(), warn_override = true)

Structure holding information about bindings which either act as references (simple substitutions) or as functions, which can be called with arguments.
This allows a small domain-specific language to be used when constructing algebraic expressions.

References are `lhs::Symbol => rhs` pairs where the left-hand side simply expands to the right-hand side during parsing. Right-hand sides which include `lhs` are supported, such that references of the form `x = x::Vector` are allowed, but will be expanded only once.
Functions are `name::Symbol => body` pairs where `rhs` must refer to their arguments with `Expr(:argument, <literal::Int>)` expressions. Recursion is not supported and will lead to a `StackOverflowError`. See [`@arg`](@ref).

Most built-in functions and symbols are implemented using this mechanism. If `warn_override` is set to true, overrides of such built-in functions will trigger a warning.
"""
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

"""
    @arg <literal::Integer>

Convenience macro to construct expressions of the form `Expr(:argument, i)` used within function definitions for [`SymbolicGA.VariableInfo`](@ref).
"""
macro arg(i)
  i > 0 || error("Argument slots must be positive integers.")
  QuoteNode(Expr(:argument, i))
end

function builtin_varinfo(sig::Signature; warn_override::Bool = true)
  refs = Dict{Symbol,Any}(
    :???? => :(1::e),
    :???? => :(1::$(Symbol(:e, join(1:dimension(sig))))),
    :??? => :left_interior_product,
    :??? => :right_interior_product,
    :??? => :left_interior_antiproduct,
    :??? => :right_interior_antiproduct,
    :??? => :geometric_antiproduct,
    :exterior_product => :???,
    :??? => :exterior_antiproduct,
    :* => :???,
    :geometric_product => :???,
    :??? => :interior_antiproduct,
    :??? => :???,
    :interior_product => :???,
    :??? => :scalar_product,
    :/ => :division,
    :inv => :inverse,
    :division => :right_division,
    :antidivision => :right_antidivision,
    :dual => :right_complement,
    :inverse_dual => :left_complement,
  )

  funcs = Dict{Symbol,Any}(
    :bulk_left_complement => :(antireverse($(@arg 1)) ??? ????),
    :bulk_right_complement => :(reverse($(@arg 1)) ??? ????),
    :weight_left_complement => :(???? ??? antireverse($(@arg 1))),
    :weight_right_complement => :(???? ??? reverse($(@arg 1))),
    :bulk => :(weight_left_complement(bulk_right_complement($(@arg 1)))),
    :weight => :(bulk_left_complement(weight_right_complement($(@arg 1)))),

    :left_interior_product => :(left_complement($(@arg 1)) ??? $(@arg 2)),
    :right_interior_product => :($(@arg 1) ??? right_complement($(@arg 2))),
    :scalar_product => :(geometric_product($(@arg 1), reverse($(@arg 1)))::Scalar),
    :left_interior_antiproduct => :($(@arg 1) ??? right_complement($(@arg 2))),
    :right_interior_antiproduct => :(left_complement($(@arg 1)) ??? $(@arg 2)),

    :bulk_norm => :(sqrt(interior_product($(@arg 1), reverse($(@arg 1))))::e),
    :weight_norm => :(sqrt(interior_antiproduct($(@arg 1), antireverse($(@arg 1))))::e??),
    :geometric_norm => :(bulk_norm($(@arg 1)) + weight_norm($(@arg 1))),
    :projected_geometric_norm => :(right_antidivision(bulk_norm($(@arg 1)), weight_norm($(@arg 1)))),
    :unitize => :(right_antidivision($(@arg 1), weight_norm($(@arg 1)))),

    :left_division => :(inverse($(@arg 1)) ??? $(@arg 2)),
    :right_division => :($(@arg 1) ??? inverse($(@arg 2))),

    :geometric_antiproduct => :(inverse_dual(geometric_product(dual($(@arg 1)), dual($(@arg 2))))),
    :exterior_antiproduct => :(inverse_dual(exterior_product(dual($(@arg 1)), dual($(@arg 2))))),
    :interior_antiproduct => :(inverse_dual(interior_product(dual($(@arg 1)), dual($(@arg 2))))),
    :antiscalar_product => :(geometric_antiproduct($(@arg 1), antireverse($(@arg 1)))::e??),
    :antiinverse => :(inverse_dual(inverse(dual($(@arg 1))))),
    :left_antidivision => :(inverse_dual(left_division(dual($(@arg 1)), dual($(@arg 2))))),
    :right_antidivision => :(inverse_dual(right_division(dual($(@arg 1)), dual($(@arg 2))))),
  )

  VariableInfo(; refs, funcs, warn_override)
end

function generate_expression(sig::Signature, ex, varinfo::Optional{VariableInfo} = nothing)
  ex = extract_expression(ex, sig, varinfo)
  ex = restructure(ex)
end

"""
    codegen_expression(sig, ex; flattening::Symbol = $(QuoteNode(DEFAULT_FLATTENING)), T = $DEFAULT_TYPE, varinfo::Optional{VariableInfo} = nothing)

Parse `ex` as an algebraic expression and generate a Julia expression which represents the corresponding computation. `sig` can be a [`SymbolicGA.Signature`](@ref) or a signature integer, tuple or tuple expression adhering to semantics of [`@ga`](@ref). See [`@ga`](@ref) for more information regarding the parsing and semantics applied to `ex`.

## Parameters
- `flattening` controls whether the components should be nested (`:nested`) or flattened (`:flattened`). In short, setting this option to `:flattened` always returns a single tuple of components, even multiple geometric entities are present in the output; while `:nested` will return a tuple of multiple elements if several geometric entities result from the computation.
- `T` specifies what type to use when reconstructing geometric entities from tuple components with [`construct`](@ref). If set to `nothing` with a `:nested` mode (default), then an appropriate [`KVector`](@ref) will be used depending on which type of geometric entity is returned; if multiple entities are present, a tuple of `KVector`s will be returned. With a `:flattened` mode, `T` will be set to `:Tuple` if unspecified.
- `varinfo` is a user-provided [`SymbolicGA.VariableInfo`](@ref) which controls what expansions are carried out on the raw Julia expression before conversion to an algebraic expression.

"""
function codegen_expression end

codegen_expression(sig_ex, ex; flattening::Symbol = DEFAULT_FLATTENING, T = DEFAULT_TYPE, varinfo::Optional{VariableInfo} = nothing) =
  codegen_expression(extract_signature(sig_ex), ex; flattening, T, varinfo)

function codegen_expression(sig::Signature, ex; flattening::Symbol = DEFAULT_FLATTENING, T = DEFAULT_TYPE, varinfo::Optional{VariableInfo} = nothing)
  in(flattening, (:flattened, :nested)) || error("Expected :flattened or :nested value for flattening keyword argument, got $flattening")
  flattening === :flattened && isnothing(T) && (T = :Tuple)
  varinfo = merge!(builtin_varinfo(sig), @something(varinfo, VariableInfo()))
  define_variables(generate_expression(sig, ex, varinfo), flattening == :flattened, T)
end

function extract_signature(ex)
  isa(ex, Integer) && return Signature(ex)
  isa(ex, Tuple) && return Signature(ex...)
  Meta.isexpr(ex, :tuple) || error("Expected tuple as signature, got $ex")
  all(isa(x, Int) for x in ex.args) || error("Expected literals in signature, got $ex")
  s = Signature(ex.args...)
end

const ADJOINT_SYMBOL = Symbol("'")

isreserved(op::Symbol) = in(op, (:???, :???, :???, :+, :??, :-, :inverse, :reverse, :antireverse, :left_complement, :right_complement, :exp))

function extract_blade_from_annotation(cache, t)
  isa(t, Symbol) || return nothing
  (t === :e || t === :e0) && return blade(cache)
  t === :e?? && return antiscalar(cache)
  m = match(r"^e(\d+)$", string(t))
  dimension(cache.sig) < 10 || error("A dimension of less than 10 is required to unambiguously refer to blades.")
  isnothing(m) && return nothing
  indices = parse.(Int, collect(m[1]))
  allunique(indices) || error("Index duplicates found in blade annotation $t.")
  maximum(indices) ??? dimension(cache.sig) || error("One or more indices exceeds the dimension of the specified space for blade $t")
  blade(cache, indices)
end

# Examples:
# - extract grade 4 for expressions x::4, x::KVector{4}, x::Quadvector.
# - extract grades 1 and 3 for expressions x::(1 + 3), x::(Vector + Trivector), x::(KVector{1} + KVector{3}), x::Multivector{1, 3}, etc.
# We might want to reduce the number of syntactically valid expressions though.
function extract_grade_from_annotation(t, sig)
  isa(t, Int) && return t
  t === :Scalar && return 0
  t === :Vector && return 1
  t === :Bivector && return 2
  t === :Trivector && return 3
  t === :Quadvector && return 4
  t === :Antiscalar && return dimension(sig)
  t === :Multivector && return collect(0:dimension(sig))
  Meta.isexpr(t, :curly, 2) && t.args[1] === :KVector && return t.args[2]::Int
  Meta.isexpr(t, :annotate_projection) && t.args[1] === :+ && return Int[extract_grade_from_annotation(t, sig) for t in @view t.args[2:end]]
  Meta.isexpr(t, :curly) && t.args[1] === :Multivector && return [extract_grade_from_annotation(arg::Int, sig) for arg in t.args[2:end]]
  Meta.isexpr(t, :tuple) && return extract_grade_from_annotation.(t.args, sig)
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

  cache = ExpressionCache(sig)
  ex = postwalk(ex) do ex
    if Meta.isexpr(ex, ADJOINT_SYMBOL)
      Expression(cache, REVERSE, ex.args[1]::Expression)
    elseif Meta.isexpr(ex, :call) && isa(ex.args[1], Symbol)
      op = ex.args[1]::Symbol
      !isreserved(op) && return ex
      args = ex.args[2:end]
      Expression(cache, Head(op), args)
    elseif Meta.isexpr(ex, :(::))
      ex, T = ex.args
      b = extract_blade_from_annotation(cache, T)
      if !isnothing(b)
        !isa(ex, Expression) || error("Blade annotations for intermediate expressions are not supported.")
        return weighted(b, ex)
      end
      g = extract_grade_from_annotation(T, sig)
      isa(ex, Expression) && return project!(ex, g)
      isa(g, Int) && return input_expression(cache, ex, g)::Expression
      isa(g, Vector{Int}) && return input_expression(cache, ex, g; flattened = !Meta.isexpr(T, (:tuple, :curly)))::Expression
      # Consider the type annotation a regular Julia expression.
      :($ex::$T)
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
  Meta.isexpr(ex, :call) && isa(ex.args[1], Symbol) || return nothing
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

function extract_weights(sig::Signature, ex, g::Int; j::Optional{Int} = nothing, offset::Optional{Int} = nothing)
  n = nelements(sig, g)
  weights = Any[]
  for i in 1:n
    push!(weights, extract_component(ex, i; j, offset, isscalar = in(g, (0, dimension(sig)))))
  end
  weights
end

function extract_component(ex, i::Int; j::Optional{Int} = nothing, offset::Optional{Int} = nothing, isscalar::Bool = false)
  isscalar && isnothing(offset) && isnothing(j) && return :($getcomponent($ex))
  !isnothing(j) && return :($getcomponent($ex, $j, $i))
  :($getcomponent($ex, $(i + something(offset, 0))))
end

function input_expression(cache::ExpressionCache, ex, g::Int; j::Optional{Int} = nothing, offset::Optional{Int} = nothing)
  blades = map(args -> blade(cache, args), combinations(1:dimension(cache.sig), g))
  weights = extract_weights(cache.sig, ex, g; j, offset)
  Expression(cache, ADDITION, Any[weighted(blade, w) for (blade, w) in zip(blades, weights)])
end

function input_expression(cache::ExpressionCache, ex, gs::AbstractVector; flattened::Bool = false)
  args = Any[]
  offset = 0
  for (j, g) in enumerate(gs)
    if flattened
      push!(args, input_expression(cache, ex, g; offset))
      offset += nelements(cache.sig, g)
    else
      push!(args, input_expression(cache, ex, g; j))
    end
  end
  Expression(cache, ADDITION, args)
end

function walk(ex::Expr, inner, outer)
  new_ex = Expr(ex.head)
  for arg in ex.args
    new = inner(arg)
    !isnothing(new) && push!(new_ex.args, new)
  end
  outer(new_ex)
end

function restructure(ex::Expression)
  @debug "Before restructuring: $(stringc(ex))"

  # Structure the different parts into multivectors and k-vectors.
  # All `+` operations are replaced with k-vector or multivector expressions.
  ex = restructure_sums(ex)
  @debug "After sum restructuring: $(stringc(ex))"

  # Add zero-factored components for blades not represented in k-vectors.
  ex = fill_kvector_components(ex)
  @debug "After k-vector component filling: $(stringc(ex))"
  @debug "After restructuring: $(stringc(ex))"
  ex
end

function to_final_expr(arg, args...)
  final = to_expr(arg, args...)
  @assert !isnothing(final) "`nothing` value detected as output element for argument $arg"
  @assert !isa(final, Expression) "Expected non-Expression element, got $ex"
  final
end

reconstructed_type(T::Nothing, sig::Signature, ex) = :($KVector{$(ex.grade::Int), $(dimension(sig))})
reconstructed_type(T, sig::Signature, ex) = T

function to_expr(cache, ex, flatten::Bool, T, variables, stop_early = false)
  if isa(ex, ID) || isa(ex, Expression)
    rhs = get(variables, ex, nothing)
    !isnothing(rhs) && return rhs
  end
  isa(ex, ID) && (ex = dereference(cache, ex))
  if isexpr(ex, MULTIVECTOR)
    if flatten
      @assert !isnothing(T)
      args = Expr.(:..., to_final_expr.(cache, ex.args, flatten, Ref(T), Ref(variables), true))
      return :($construct($(reconstructed_type(T, ex.cache.sig, ex)), $(Expr(:tuple, args...))))
    else
      return Expr(:tuple, to_final_expr.(cache, ex, flatten, Ref(T), Ref(variables))...)
    end
  end
  isexpr(ex, FACTOR) && return to_final_expr(cache, ex[1], flatten, T, variables)
  isexpr(ex, KVECTOR) && return :($construct($(reconstructed_type(T, ex.cache.sig, ex)), $(Expr(:tuple, to_final_expr.(cache, ex, flatten, Ref(T), Ref(variables))...))))
  isexpr(ex, BLADE) && return 1
  if isexpr(ex, GEOMETRIC_PRODUCT)
    @assert isweightedblade(ex)
    return to_final_expr(cache, ex[1]::Expression, flatten, T, variables)
  end
  ex === Zero() && return 0
  isa(ex, Expr) && !stop_early && return Expr(ex.head, to_expr.(cache, ex.args, flatten, Ref(T), Ref(variables))...)
  ex
end

struct ExecutionGraph
  g::SimpleDiGraph{Int}
  exs::Dict{Union{ID,Expression},Int}
  exs_inv::Dict{Int,Union{ID,Expression}}
end

ExecutionGraph() = ExecutionGraph(SimpleDiGraph{Int}(), Dict(), Dict())

function get_vertex!(g::ExecutionGraph, ex::Union{ID,Expression})
  v = get(g.exs, ex, nothing)
  !isnothing(v) && return v
  add_vertex!(g.g)
  v = nv(g.g)
  g.exs[ex] = v
  g.exs_inv[v] = ex
  v
end

function add_node_uses!(uses, g::ExecutionGraph, i, ex, T)
  j = 0
  if (isa(ex, ID) || isa(ex, Expression))
    uses[ex] = 1 + get!(uses, ex, 0)
    j = get_vertex!(g, ex)
    add_edge!(g.g, i, j)
  end
  if isa(ex, ID) && T === Expression
    ex = dereference(ex, ex)
    if isa(ex, Expr)
      for arg in ex.args
        add_node_uses!(uses, g, j, arg, Expr)
      end
    end
  elseif isa(ex, Expression) && T === Expr
    for arg in ex
      add_node_uses!(uses, g, j, arg, Expression)
    end
  end
  nothing
end

function define_variables(ex::Expression, flatten::Bool, T)
  variables = Dict{Union{ID,Expression},Symbol}()
  uses = Dict{Union{ID,Expression},Int}()
  g = ExecutionGraph()
  add_node_uses!(uses, g, get_vertex!(g, ex), ex, Expr)
  rem_edge!(g.g, 1, 1)
  definitions = Expr(:block)

  # Define variables in a first pass.
  for (x, count) in pairs(uses)
    variables[x] = gensym("SymbolicGA")
  end

  current_variables = Dict{Union{ID,Expression},Symbol}()

  # Recursively generate code using the previously defined variables.
  for v in reverse(topological_sort_by_dfs(g.g))
    x = g.exs_inv[v]
    code = to_final_expr(ex.cache, x, flatten, T, current_variables)
    var = variables[x]
    push!(definitions.args, Expr(:(=), var, code))
    current_variables[x] = var
  end
  definitions
end
