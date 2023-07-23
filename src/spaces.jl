"""
    @geometric_space name sig quote
      n = 1.0::e4 + 1.0::e5
      n̄ = (-0.5)::e4 + 0.5::e5
      ... # other definitions
    end [warn_override = true]

Generate a macro `@name` which defines a geometric space with signature `sig` along with optional user-provided definitions.

The resulting macro will have two methods:
- `@name ex`, which acts as a standard `@ga sig ex` (but with potential extra definitions).
- `@name T ex` which wraps the result into type `T`, just like `@ga sig T ex` would (see [`@ga`](@ref)).

If definitions are provided as an `Expr`, they will be parsed into [`Bindings`](@ref) and added to the default bindings. If definitions already are [`Bindings`](@ref), they will be used as is.
`warn_override` can be set to false if you purposefully intend to purposefully override some of the default bindings.
"""
macro geometric_space(name::Symbol, sig_ex, definitions = nothing, warn_override = :(warn_override = true))
  warn_override = Meta.isexpr(warn_override, :(=), 2) && warn_override.args[1] == :warn_override ? warn_override.args[2]::Bool : throw(ArgumentError("Expected `warn_override = <true|false>` as last argument, got $(repr(warn_override))"))
  if isnothing(definitions)
    bindings = nothing
  else
    ex = Core.eval(__module__, definitions)
    if isa(ex, Expr)
      bindings = merge!(default_bindings(), parse_bindings(ex; warn_override))
    elseif isa(ex, Bindings)
      bindings = ex
    else
      throw(ArgumentError("Expected `definitions` argument to be a Julia expression or a `SymbolciGA.Bindings`, got $(typeof(ex))"))
    end
  end

  docstring = """
      @$name(T, ex)
      @$name(ex)

  Macro generated via `SymbolicGA.@geometric_space` with signature `$sig_ex`
  """
  if !isnothing(definitions) && Meta.isexpr(ex, :block)
    defs = Expr[]
    for line in ex.args
      isa(line, LineNumberNode) && continue
      if Meta.isexpr(line, :(=)) && Meta.isexpr(line.args[1], :call)
        call, body = line.args
        body = Meta.isexpr(body, :block, 2) && isa(body.args[1], LineNumberNode) ? body.args[2] : body
        line = :($call = $body)
        push!(defs, line)
      end
    end
    docstring *= " and the following definitions:\n$(join("\n- ```julia\n  " .* string.(defs) .* "\n  ```"))"
  elseif !isnothing(bindings)
    docstring *= " and the following bindings: \n\n$bindings"
  end

  var = Symbol("@$name")
  macros = quote
    Core.@__doc__ macro $name end
    macro $name(T, ex)
      ex = codegen_expression($sig_ex, ex; T, bindings = $bindings)
      esc(ex)
    end
    macro $name(ex)
      ex = Expr(:macrocall, $var, __source__, nothing, ex)
      esc(ex)
    end
    $with_logger($NullLogger()) do
      Core.@doc $docstring * '\n' * repr(Core.@doc $var) $var
    end
  end
  # Adjust `LineNumberNode`s to include the callsite in stacktraces.
  for i in (4, 6)
    def = macros.args[i]
    @assert Meta.isexpr(def, :macro)
    body = def.args[2]
    @assert Meta.isexpr(body, :block) body
    @assert isa(body.args[2], LineNumberNode)
    body.args[2] = __source__
  end
  esc(macros)
end

@geometric_space pga2 (2, 0, 1) quote
  embed(x) = x[1]::e1 + x[2]::e2
  magnitude2(x) = x ⦿ x
  point(x) = embed(x) + 1.0::e3
end

@geometric_space pga3 (3, 0, 1) quote
  embed(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
  magnitude2(x) = x ⦿ x
  point(x) = embed(x) + 1.0::e4
end

"""
3D Conformal Geometric Algebra.

!!! warning
    This functionality is experimental and will likely be subject to change in the future.
    It is not recommended for use beyond prototyping and playing around.
"""
@geometric_space cga3 (4, 1) quote
  n = 1.0::e4 + 1.0::e5
  n̄ = (-0.5)::e4 + 0.5::e5
  n̅ = n̄ # n\bar !== n\overbar but they display exactly the same.
  embed(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
  magnitude2(x) = x ⦿ x
  point(x) = (embed(x) + (0.5::Scalar ⟑ magnitude2(embed(x))) ⟑ n + n̄)::Vector
  weight(X) = -X ⋅ n
  unitize(X) = X / weight(X)
  radius2(X) = (magnitude2(X) / magnitude2(X ∧ n))::Scalar
  center(X) = X ⟑ n ⟑ X
  # For spheres `S` defined as vectors, and points `X` defined as vectors as well.
  distance(S, X) = unitize(S) ⋅ unitize(X)
end warn_override = false
