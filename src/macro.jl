const DEFAULT_TYPE = nothing

"""
    @ga <sig> <T> <ex>
    @ga <sig> <ex>

Generate Julia code which implements the computation of geometric elements from `ex` in an algebra defined by a signature `sig` (see [`SymbolicGA.Signature`](@ref)).

Supported syntax:
- `sig`: Integer literal or tuple of 1, 2 or 3 integer literals corresponding to the number of positive, negative and degenerate basis vectors respectively, where unspecified integers default to zero. May also be a string literal of the form `<+++--𝟎>` where the number of `+`, `-` and `𝟎` correspond to the nmuber of positive, negative and degenerate basis vectors respectively.
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
  - Function arguments must not be reassigned; it is assumed that any occurence of symbols declared as arguments will reference these arguments. For example, `f(x, y) = x + y` assumes that `x + y` actually means "perform `+` on the first and second function argument". Therefore, `f(x, y) = (x = 2; x) + y` will be likely to cause bugs. To alleviate this restriction, use [`codegen_expression`](@ref) with a suitable [`SymbolicGA.Bindings`] with function entries that contain specific calls to `:(\$(@arg <i>)).

## Binding expansion

References and functions are expanded in a fairly straightforward copy-paste manner, where references are replaced with their right-hand side and function calls with their bodies with their arguments interpolated. Simple checks are put in place to allow for self-referencing bindings for references, such as `x = x::T`, leading to a single expansion of such a pattern in the corresponding expression subtree.

See [`SymbolicGA.Bindings`](@ref) for more information regarding the expansion of such variables and functions.

# Algebraic evaluation

Type annotations may either:
- Specify what type of geometric entity an input should be considered as, where components are then picked off with `getcomponent`.
- Request the projection of an intermediate expression over one or multiple grades.
"""
macro ga(sig_ex, args...)
  T, ex = parse_arguments(args)
  ex = codegen_expression(sig_ex, ex; T)
  propagate_source(__source__, esc(ex))
end

function parse_arguments(args)
  length(args) ≤ 2 || throw(MethodError(var"@ga", args))
  length(args) == 2 ? args : (DEFAULT_TYPE, args[1])
end

propagate_source(__source__, ex) = Expr(:block, LineNumberNode(__source__.line, __source__.file), ex)

"""
    Bindings(; refs = Dict{Symbol,Any}(), funcs = Dict{Symbol,Any}(), warn_override = true)

Structure holding information about bindings which either act as references (simple substitutions) or as functions, which can be called with arguments.
This allows a small domain-specific language to be used when constructing algebraic expressions.

References are `lhs::Symbol => rhs` pairs where the left-hand side simply expands to the right-hand side during parsing. Right-hand sides which include `lhs` are supported, such that references of the form `x = x::Vector` are allowed, but will be expanded only once.
Functions are `name::Symbol => body` pairs where `rhs` must refer to their arguments with `Expr(:argument, <literal::Int>)` expressions. Recursion is not supported and will lead to a `StackOverflowError`. See [`@arg`](@ref).

Most built-in functions and symbols are implemented using this mechanism. If `warn_override` is set to true, overrides of such built-in functions will trigger a warning.
"""
struct Bindings
  refs::Dict{Symbol,Any}
  funcs::Dict{Symbol,Any}
  warn_override::Bool
  ref_sourcelocs::Dict{Symbol,LineNumberNode}
  func_sourcelocs::Dict{Symbol,LineNumberNode}
end

Bindings(; refs = Dict(), funcs = Dict(), warn_override::Bool = true) = Bindings(refs, funcs, warn_override, Dict(), Dict())

function Base.merge!(x::Bindings, y::Bindings)
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

Convenience macro to construct expressions of the form `Expr(:argument, i)` used within function definitions for [`SymbolicGA.Bindings`](@ref).
"""
macro arg(i)
  i > 0 || error("Argument slots must be positive integers.")
  QuoteNode(Expr(:argument, i))
end

"""
[`Bindings`](@ref) included by default in [`@ga`](@ref).

By default, any user-defined symbol overriding a symbol defined here will trigger a warning; set `warn_override = false` to disable this.
"""
function builtin_bindings(; warn_override::Bool = true)
  refs = Dict{Symbol,Any}(
    :𝟏 => :(1::e),
    :𝟙 => :(1::e̅),
    :⟑ => :geometric_product,
    :⩒ => :geometric_antiproduct,
    :∧ => :exterior_product,
    :outer_product => :exterior_product,
    :∨ => :exterior_antiproduct,
    :outer_antiproduct => :exterior_antiproduct,
    :⬤ => :interior_product,
    :● => :interior_product,
    :⋅ => :interior_product,
    :inner_product => :interior_product,
    :○ => :interior_antiproduct,
    :inner_antiproduct => :interior_antiproduct,
    :⦿ => :scalar_product,
    :⊣ => :left_interior_product,
    :⨼ => :left_interior_antiproduct,
    :⊢ => :right_interior_product,
    :⨽ => :right_interior_antiproduct,
    :× => :commutator_product,
    :/ => :division,
    :inv => :inverse,
    :dual => :right_complement,
    :inverse_dual => :left_complement,
    :<< => :versor_product,
  )

  @static if VERSION ≥ v"1.10-DEV"
    refs[Symbol("⟇")] = :geometric_antiproduct
  end

  funcs = Dict{Symbol,Any}(
    :bulk_left_complement => :(geometric_product(antireverse($(@arg 1)), 𝟙)),
    :bulk_right_complement => :(geometric_product(reverse($(@arg 1)), 𝟙)),
    :weight_left_complement => :(geometric_antiproduct(𝟏, antireverse($(@arg 1)))),
    :weight_right_complement => :(geometric_antiproduct(𝟏, reverse($(@arg 1)))),
    :bulk => :(weight_left_complement(bulk_right_complement($(@arg 1)))),
    :weight => :(bulk_left_complement(weight_right_complement($(@arg 1)))),

    :left_interior_product => :(exterior_antiproduct(left_complement($(@arg 1)), $(@arg 2))),
    :right_interior_product => :(exterior_antiproduct($(@arg 1), right_complement($(@arg 2)))),
    :scalar_product => :(geometric_product($(@arg 1), reverse($(@arg 1)))::Scalar),
    :left_interior_antiproduct => :(exterior_product($(@arg 1), right_complement($(@arg 2)))),
    :right_interior_antiproduct => :(exterior_product(left_complement($(@arg 1)), $(@arg 2))),

    :bulk_norm => :(sqrt(interior_product($(@arg 1), reverse($(@arg 1))))::e),
    :weight_norm => :(sqrt(interior_antiproduct($(@arg 1), antireverse($(@arg 1))))::e̅),
    :geometric_norm => :(bulk_norm($(@arg 1)) + weight_norm($(@arg 1))),
    :projected_geometric_norm => :(antidivision(bulk_norm($(@arg 1)), weight_norm($(@arg 1)))),
    :unitize => :(antidivision($(@arg 1), weight_norm($(@arg 1)))),

    :division => :(geometric_product($(@arg 1), inverse($(@arg 2)))),
    :antidivision => :(inverse_dual(division(dual($(@arg 1)), dual($(@arg 2))))),

    :geometric_antiproduct => :(inverse_dual(geometric_product(dual($(@arg 1)), dual($(@arg 2))))),
    :exterior_antiproduct => :(inverse_dual(exterior_product(dual($(@arg 1)), dual($(@arg 2))))),
    :interior_antiproduct => :(inverse_dual(interior_product(dual($(@arg 1)), dual($(@arg 2))))),
    :antiscalar_product => :(geometric_antiproduct($(@arg 1), antireverse($(@arg 1)))::e̅),
    :antiinverse => :(inverse_dual(inverse(dual($(@arg 1))))),

    :versor_product => :(geometric_product($(@arg 2), $(@arg 1), inverse($(@arg 2)))),
  )

  Bindings(; refs, funcs, warn_override)
end

function generate_expression(sig::Signature, ex, bindings::Bindings = builtin_bindings(); factorize = true, optimize = true)
  ex, flattened = extract_expression(ex, sig, bindings)
  ex = restructure(ex)
  factorize && factorize!(ex)
  optimize && (ex = optimize!(ex))
  ex, flattened
end

"""
    codegen_expression(sig, ex; T = $DEFAULT_TYPE, bindings::Optional{Bindings} = nothing)

Parse `ex` as an algebraic expression and generate a Julia expression which represents the corresponding computation. `sig` can be a [`SymbolicGA.Signature`](@ref), a signature integer or a signature string, tuple or tuple expression adhering to semantics of [`@ga`](@ref). See [`@ga`](@ref) for more information regarding the parsing and semantics applied to `ex`.

## Parameters
- `T` specifies what type to use when reconstructing geometric entities from tuple components with [`construct`](@ref). If set to `nothing` and the result is in a non-flattened form (i.e. not annotated with an annotation of the type `\$ex::(0 + 2)`), then an appropriate [`KVector`](@ref) will be used depending on which type of geometric entity is returned; if multiple entities are present, a tuple of `KVector`s will be returned. If the result is in a flattened form, `T` will be set to `:Tuple` if unspecified.
- `bindings` is a user-provided [`SymbolicGA.Bindings`](@ref) which controls what expansions are carried out on the raw Julia expression before conversion to an algebraic expression.

"""
function codegen_expression end

codegen_expression(sig_ex, ex; T = DEFAULT_TYPE, bindings::Optional{Bindings} = nothing) =
  codegen_expression(extract_signature(sig_ex), ex; T, bindings)

function codegen_expression(sig::Signature, ex; T = DEFAULT_TYPE, bindings::Optional{Bindings} = nothing)
  bindings = merge!(builtin_bindings(), @something(bindings, Bindings()))
  generated, flattened = generate_expression(sig, ex, bindings)
  flattened && isnothing(T) && (T = :Tuple)
  define_variables(generated, flattened, T)
end

function extract_signature(ex)
  (isa(ex, Integer) || isa(ex, AbstractString)) && return Signature(ex)
  isa(ex, Tuple) && return Signature(ex...)
  Meta.isexpr(ex, :tuple) || error("Expected tuple as signature, got $ex")
  all(isa(x, Int) for x in ex.args) || error("Expected literals in signature, got $ex")
  s = Signature(ex.args...)
end

const ADJOINT_SYMBOL = Symbol("'")

isreserved(op::Symbol) = in(op, (:+, :-, :geometric_product, :exterior_product, :interior_product, :commutator_product, :inverse, :reverse, :antireverse, :left_complement, :right_complement, :exp))

function extract_blade_from_annotation(cache, t)
  isa(t, Symbol) || return nothing
  (t === :e || t === :e0) && return blade(cache)
  t in (:e̅, :ē) && return antiscalar(cache)
  if startswith(string(t), "e_")
    indices = parse.(Int, split(string(t), '_')[2:end])
  else
    m = match(r"^e(\d+)$", string(t))
    dimension(cache.sig) < 10 || error("A dimension of less than 10 is required to unambiguously refer to blades.")
    isnothing(m) && return nothing
    indices = parse.(Int, collect(m[1]))
  end
  allunique(indices) || error("Index duplicates found in blade annotation $t.")
  maximum(indices) ≤ dimension(cache.sig) || error("One or more indices exceeds the dimension of the specified space for blade $t")
  blade(cache, indices)
end

function extract_grade_from_annotation(t, sig)
  isa(t, Int) && return t
  t === :Scalar && return 0
  t === :Vector && return 1
  t === :Bivector && return 2
  t === :Trivector && return 3
  t === :Quadvector && return 4
  t === :Antiscalar && return dimension(sig)
  t === :Multivector && return collect(0:dimension(sig))
  Meta.isexpr(t, :annotate_projection) && t.args[1] === :+ && return Int[extract_grade_from_annotation(t, sig) for t in @view t.args[2:end]]
  Meta.isexpr(t, :tuple) && return extract_grade_from_annotation.(t.args, sig)
  error("Unknown grade projection for algebraic element $t")
end

function extract_expression(ex, sig::Signature, bindings::Bindings)
  # Shield interpolated regions in a `QuoteNode` from expression processing.
  ex = prewalk(ex) do ex
    Meta.isexpr(ex, :$) && return QuoteNode(ex.args[1])
    ex
  end

  ex2 = expand_variables(ex, bindings)
  @debug "After variable expansion: $(stringc(ex2))"

  # Make sure calls in annotations are not interpreted as actual operations.
  ex3 = prewalk(ex2) do ex
    if Meta.isexpr(ex, :(::)) && Meta.isexpr(ex.args[2], :call)
      ex.args[2] = Expr(:annotate_projection, ex.args[2].args...)
    end
    ex
  end

  cache = ExpressionCache(sig)
  flattened = Meta.isexpr(ex3, :(::)) && begin
    _, T = ex3.args
    Meta.isexpr(T, :annotate_projection) && T.args[1] === :+
  end
  ex4 = postwalk(ex3) do ex
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
        if isa(ex, Expression)
          # Allow blade projections only when it is equivalent to projecting on a grade.
          isempty(basis_vectors(b)) && return project!(ex, 0)
          basis_vectors(b) == collect(1:dimension(sig)) && return project!(ex, dimension(sig))
          error("Blade annotations are not supported for specifying projections.")
        end
        return weighted(b, ex)
      end
      g = extract_grade_from_annotation(T, sig)
      isa(ex, Expression) && return project!(ex, g)
      isa(g, Int) && return input_expression(cache, ex, g)::Expression
      isa(g, Vector{Int}) && return input_expression(cache, ex, g; flattened = !Meta.isexpr(T, :tuple))::Expression
      # Consider the type annotation a regular Julia expression.
      :($ex::$T)
    elseif isa(ex, QuoteNode) && !isa(ex.value, Symbol)
      # Unwrap quoted expressions, except for symbols for which quoting is the only
      # way to poss a literal around.
      ex.value
    else
      ex
    end
  end

  isa(ex4, Expression) || error("Could not fully extract expression: $ex4\n\nOutermost expression has head $(ex4.head) and arguments $(ex4.args)\n")
  ex4, flattened
end

sourceloc(ln::Nothing) = ""
sourceloc(ln::LineNumberNode) = string(" around ", ln.file, ':', ln.line)

function parse_bindings(ex::Expr; warn_override::Bool = true)
  @assert Meta.isexpr(ex, :block)
  parse_bindings(ex.args; warn_override)
end
function parse_bindings(exs; warn_override::Bool = true)
  bindings = Bindings(; warn_override)
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
      haskey(bindings.funcs, name) && @warn "Redefinition of user-defined function `$name`$(sourceloc(last_line)) (only one method is allowed)."
      bindings.funcs[name] = body
      !isnothing(last_line) && (bindings.func_sourcelocs[name] = last_line)
      continue
    end

    if Meta.isexpr(ex, :(::)) && isa(ex.args[1], Symbol)
      # Type declaration.
      var, T = ex.args
      ex = :($var = $var::$T)
    end

    !Meta.isexpr(ex, :(=), 2) && error("Non-final expression parsed without a left-hand side assignment", sourceloc(last_line), '.')
    lhs, rhs = ex.args
    haskey(bindings.refs, lhs) && @warn "Redefinition of user-defined variable `$lhs`$(sourceloc(last_line))."
    bindings.refs[lhs] = rhs
    !isnothing(last_line) && (bindings.ref_sourcelocs[lhs] = last_line)
  end
  bindings
end

"""
Expand variables from a block expression, yielding a final expression where all variables were substitued with their defining expression.
"""
function expand_variables(ex, bindings::Bindings)
  !Meta.isexpr(ex, :block) && (ex = Expr(:block, ex))
  rhs = nothing

  i = findlast(x -> !isa(x, LineNumberNode), eachindex(ex.args))
  isnothing(i) && error("No input expression could be parsed.")
  parsed_bindings = parse_bindings(ex.args[1:(i - 1)])
  bindings = merge!(deepcopy(bindings), parsed_bindings)

  last_ex = ex.args[i]
  rhs = Meta.isexpr(last_ex, :(=)) ? last_ex.args[2] : last_ex
  # Expand references and function calls.
  res = postwalk(ex -> expand_subtree(ex, bindings.refs, bindings.funcs, Set{Symbol}()), rhs)
  !isa(res, Expr) && error("Expression resulted in a trivial RHS before simplifications: $(repr(res))")
  res
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

function extract_weights(cache::ExpressionCache, ex, g::Int; j::Optional{Int} = nothing, offset::Optional{Int} = nothing)
  n = nelements(cache.sig, g)
  weights = Any[]
  for i in 1:n
    push!(weights, extract_component(cache, ex, i; j, offset, isscalar = in(g, (0, dimension(cache.sig)))))
  end
  weights
end

function extract_component(cache::ExpressionCache, ex, i::Int; j::Optional{Int} = nothing, offset::Optional{Int} = nothing, isscalar::Bool = false)
  isscalar && isnothing(offset) && isnothing(j) && return Expression(cache, COMPONENT, ex)
  !isnothing(j) && return Expression(cache, COMPONENT, ex, j, i)
  Expression(cache, COMPONENT, ex, i + something(offset, 0))
end

function input_expression(cache::ExpressionCache, ex, g::Int; j::Optional{Int} = nothing, offset::Optional{Int} = nothing)
  blades = map(args -> blade(cache, args), combinations(1:dimension(cache.sig), g))
  weights = extract_weights(cache, ex, g; j, offset)
  Expression(cache, ADDITION, Term[weighted(blade, w) for (blade, w) in zip(blades, weights)])
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
  @assert !isa(final, Expression) "Expected non-Expression element, got $final"
  final
end

reconstructed_type(T::Nothing, sig::Signature, ex) = :($KVector{$(ex.grade::Int), $(dimension(sig))})
reconstructed_type(T, sig::Signature, ex) = T

function to_expr(cache, ex, flatten::Bool, T, variables, stop_early = false)
  if isa(ex, Term)
    rhs = get(variables, ex, nothing)
    !isnothing(rhs) && return rhs
  end
  isa(ex, ID) && (ex = dereference(cache, ex))
  isa(ex, Expression) && isscalar(ex.head) && return Expr(:call, scalar_function(ex.head), to_final_expr.(cache, ex.args, flatten, Ref(T), Ref(variables), true)...)
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

function scalar_function(head::Head)
  head === COMPONENT && return getcomponent
  head === SCALAR_SQRT && return sqrt
  head === SCALAR_ABS && return abs
  head === SCALAR_NAN_TO_ZERO && return nan_to_zero
  head === SCALAR_INVERSE && return inv
  head === SCALAR_COS && return cos
  head === SCALAR_SIN && return sin
  head === SCALAR_COSH && return cosh
  head === SCALAR_SINH && return sinh
  head === SCALAR_DIVISION && return /
  head === SCALAR_PRODUCT && return *
  head === SCALAR_ADDITION && return +
  isscalar(head) && error("Head `$head` does not have a corresponding scalar function")
  error("Expected head `$head` to be denoting a scalar expression")
end

nan_to_zero(x) = ifelse(isnan(x), zero(x), x)

struct ExecutionGraph
  g::SimpleDiGraph{Int}
  exs::Dict{Union{ID,Expression},Int}
  exs_inv::Dict{Int,Union{ID,Expression}}
end

ExecutionGraph() = ExecutionGraph(SimpleDiGraph{Int}(), Dict(), Dict())

function ExecutionGraph(ex)
  uses = Dict{Term,Int}()
  g = ExecutionGraph()
  add_node_uses!(uses, g, ex.cache, get_vertex!(g, ex), ex)
  rem_edge!(g.g, 1, 1)
  g
end

traverse(g::ExecutionGraph) = reverse(topological_sort_by_dfs(g.g))

function get_vertex!(g::ExecutionGraph, ex::Union{ID,Expression})
  v = get(g.exs, ex, nothing)
  !isnothing(v) && return v
  add_vertex!(g.g)
  v = nv(g.g)
  g.exs[ex] = v
  g.exs_inv[v] = ex
  v
end

isexpandable(deref) = isa(deref, Expr) || isa(deref, Expression) && isscalar(deref.head)

function add_node_uses!(uses, g::ExecutionGraph, cache, i, ex)
  j = 0
  deref = dereference(cache, ex)
  if (isa(ex, Expression) || isa(ex, ID) && isexpandable(deref))
    uses[ex] = 1 + get!(uses, ex, 0)
    j = get_vertex!(g, ex)
    add_edge!(g.g, i, j)
  end
  if isa(ex, Expression)
    for arg in ex
      add_node_uses!(uses, g, cache, j, arg)
    end
  elseif isa(ex, ID) && isexpandable(deref)
    for arg in deref.args
      add_node_uses!(uses, g, cache, j, arg)
    end
  elseif isa(ex, Expr)
    for arg in ex.args
      if isa(arg, Expr)
        add_node_uses!(uses, g, cache, i, arg)
      elseif isa(arg, Expression)
        add_node_uses!(uses, g, cache, i, arg)
      end
    end
  end
  nothing
end

function define_variables(ex::Expression, flatten::Bool, T)
  variables = Dict{Term,Symbol}()
  g = ExecutionGraph(ex)
  definitions = Expr(:block)

  # Recursively generate code using the previously defined variables.
  for v in traverse(g)
    x = g.exs_inv[v]
    code = to_final_expr(ex.cache, x, flatten, T, variables)
    if isa(code, Expr)
      var = gensym()
      push!(definitions.args, Expr(:(=), var, code))
      variables[x] = var
    end
  end
  definitions
end
