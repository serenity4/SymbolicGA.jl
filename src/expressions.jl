const GradeInfo = Union{Int,Vector{Int}}

mutable struct Expression
  head::Symbol
  grade::GradeInfo
  args::Vector{Any}
  function Expression(head::Symbol, args::AbstractVector; simplify = true, grade = nothing)
    ex = new()
    # Aliases.
    head === :⋅ && (head = :●)
    head === :* && (head = :⟑)
    ex.head = head
    ex.args = args
    !isnothing(grade) && (ex.grade = grade)
    !simplify && return ex
    simplify!(ex, nothing)
  end
  Expression(head::Symbol, args...; simplify = true, grade = nothing) = Expression(head, collect(Any, args); simplify, grade)
end
# Can mutate arguments.
simplified(sig, head::Symbol, args...) = simplify!(Expression(head, args...; simplify = false), sig)
simplified(head::Symbol, args...) = simplified(nothing, head, args...)

function simplify!(ex::Expression, sig::Optional{Signature} = nothing)
  (; head, args) = ex
  # Simplify nested factor expressions.
  head === :factor && isexpr(args[1], :factor) && return simplify!(args[1]::Expression, sig)
  # Apply scalar and blade inversions.
  head === :inverse && isblade(args[1]) && return reverse(args[1])
  if head === :inverse && isweightedblade(args[1])
    fac_arg = inverse(args[1][1][1])
    return weighted(reverse(args[1][2]), fac_arg)
  end

  if head === :blade
    # Sort basis vectors.
    if !issorted(args)
      perm = sortperm(args)
      fac = isodd(parity(perm)) ? -1 : 1
      return simplified(sig, :⟑, simplified(sig, :factor, fac), simplified(sig, :blade, args[perm]))
    end

    # Apply metric.
    if !allunique(args)
      sig::Signature
      last = nothing
      fac = 1
      i = 1
      while length(args) ≥ 2
        i > lastindex(args) && break
        new = args[i]
        if !isnothing(last)
          if new == last
            m = metric(sig, last)
            iszero(m) && return scalar(0)
            fac *= m
            length(args) == 2 && return scalar(fac)
            deleteat!(args, i)
            deleteat!(args, i - 1)
            i = max(i - 2, 1)
            last = i < firstindex(args) ? nothing : args[i]
          else
            last = new
          end
        else
          last = new
        end
        i += 1
      end
      return weighted(simplified(sig, :blade, args), fac)
    end
  end

  if head === :-
    # Simplify unary minus operator to a multiplication with -1.
    length(args) == 1 && return weighted(args[1], -1)
    # Transform binary minus expression into an addition.
    @assert length(args) == 2
    return simplified(sig, :+, args[1], -args[2])
  end

  # Simplify whole expression to zero if a product term is zero.
  if head === :⟑ && any(isexpr(x, :factor) && x[1] == 0 for x in args)
    any(!isexpr(x, :factor) && !isexpr(x, :blade, 0) for x in args) && @debug "Non-factor expression annihilated by a zero multiplication term" args
    return factor(0)
  end

  # Remove unit elements for multiplication and addition.
  fac = remove_unit_elements!(args, head)
  !isnothing(fac) && return scalar(fac)

  if head === :⩒ && length(args) == 2 && all(isexpr(:blade), args)
    x, y = args[1]::Expression, args[2]::Expression
    return blade_complement(sig, simplified(sig, :⟑, blade_complement(sig, x), blade_complement(sig, y)))
  end

  if in(head, (:⟑, :⩒, :+))
    length(args) == 1 && return args[1]

    # Disassociate ⟑, ⩒ and +.
    any(isexpr(head), args) && return disassociate1(args, head, sig)
  end

  if in(head, (:⟑, :⩒))
    # Collapse factors into one and put it at the front.
    nf = count(isexpr(:factor), args)
    if nf ≥ 2
      # Collapse all factors into one at the front.
      factors, nonfactors = filter(isexpr(:factor), args), filter(!isexpr(:factor), args)
      fac = collapse_factors(*, factors)
      length(args) == nf && return fac
      args = [fac; nonfactors]
    elseif nf == 1 && !isexpr(args[1], :factor)
      # Put the factor at the front.
      i = findfirst(isexpr(:factor), args)
      fac = args[i]::Expression
      deleteat!(args, i)
      pushfirst!(args, fac)
      return simplified(sig, :⟑, args)
    end

    # Collapse all blades as one.
    if head === :⟑
      # Collapse all bases and blades into a single blade.
      nb = count(isexpr(:blade), args)
      if nb > 1
        n = length(args)
        blade_args = []
        for i in reverse(eachindex(args))
          x = args[i]
          isexpr(x, :blade) || continue
          for arg in reverse(x.args)
            pushfirst!(blade_args, arg::Int)
          end
          deleteat!(args, i)
        end
        ex = blade(sig, blade_args)
        # Return the blade if all the terms were collapsed.
        nb == n && return ex
        return simplified(sig, :⟑, Any[args; ex])
      end
    end
  end

  # Simplify addition of factors.
  head === :+ && all(isexpr(:factor), args) && return collapse_factors(+, args)

  # Group blade components over addition.
  if head === :+
    indices = findall(x -> isblade(x) || isweightedblade(x), args)
    if !isempty(indices)
      blades = args[indices]
      if !allunique(basis_vectors(b) for b in blades)
        blade_weights = Dict{Vector{Int},Expression}()
        for b in blades
          vecs = basis_vectors(b)
          weight = isweightedblade(b) ? b.args[1] : factor(1)
          blade_weights[vecs] = haskey(blade_weights, vecs) ? blade_weights[vecs] + weight : weight
        end
        new_args = args[setdiff(eachindex(args), indices)]
        append!(new_args, weight ⟑ blade(vecs) for (vecs, weight) in blade_weights)
        return simplified(:+, new_args)
      end
    end
  end


  # Simplify -1 factors.
  head === :factor && (args[1] = simplify_negations(args[1]))

  # Check that arguments make sense.
  n = expected_nargs(head)
  !isnothing(n) && @assert length(args) == n "Expected $n arguments for expression $head, $n were provided\nArguments: $args"
  @assert head === :blade || !isempty(args)
  head === :blade && @assert all(isa(i, Int) for i in args)
  if head === :project
    @assert isa(args[1], Int) && isa(args[2], Expression)
    !isnothing(sig) && @assert 0 ≤ args[1] ≤ dimension(sig)
  end

  # Distribute products over addition.
  head in (:⟑, :⩒, :∧, :∨, :●, :○, :⦿, :×) && any(isexpr(arg, :+) for arg in args) && return distribute1(ex, head, sig)

  # Eagerly apply projections.
  head === :project && return project!(args[2]::Expression, args[1]::GradeInfo)

  # Eagerly apply reversions.
  head in (:reverse, :antireverse) && return apply_reverse_operators(ex, sig)

  # Expand common operators.
  # ========================

  # The exterior (anti)product is associative, no issues there.
  if head in (:∧, :∨)
    anti = head === :∨
    n = sum(anti ? x -> antigrade(sig::Signature, x::Expression)::Int : x -> grade(x::Expression)::Int, args)
    return project!(simplified(sig, :⟑, args), n)
  end

  if head === :●
    # The inner product must have only two operands, as no associativity law is available to derive a canonical binarization.
    # Homogeneous vectors are expected, so the grade should be known.
    r, s = grade(args[1]::Expression)::Int, grade(args[2]::Expression)::Int
    if (iszero(r) || iszero(s))
      (!iszero(r) || !iszero(s)) && @debug "Non-scalar expression annihilated in inner product with a scalar"
      return scalar(0)
    end
    return project!(simplified(sig, :⟑, args), abs(r - s))
  end

  head === :⦿ && return project!(simplified(sig, :⟑, args), 0)

  if head === :×
    # The commutator product must have only two operands, as no associativity law is available to derive a canonical binarization.
    a, b = args[1]::Expression, args[2]::Expression
    return weighted(simplified(sig, :-, simplified(sig, :⟑, a, b), simplified(sig, :⟑, b, a)), 0.5)
  end

  if head === :inverse
    a = args[1]::Expression
    @assert a ≠ scalar(0)
    denom = simplified(sig, :⦿, a, a)
    @assert isscalar(denom)
    return reverse(a) / denom
  end

  if head === :/
    a, b = args[1]::Expression, args[2]::Expression
    return simplified(sig, :⟑, a, simplified(sig, :inverse, b))
  end

  ex.grade = infer_grade(head, args)::GradeInfo
  ex.args = args

  ex
end

function remove_unit_elements!(args, head)
  if head === :⟑
    filter!(x -> !isexpr(x, :factor) || x[1] !== 1, args)
    isempty(args) && return 1
  elseif head === :+
    filter!(x -> !isexpr(x, :factor) || x[1] !== 0, args)
    isempty(args) && return 0
  end
  nothing
end

function collapse_factors(f::F, args) where {F<:Function}
  @assert f === (*) || f === (+)
  unit = f === (*) ? one : zero
  isunit = f === (*) ? isone : iszero
  opaque_factors = Any[]
  value = unit(Int64)
  for fac in args
    if isopaquefactor(fac)
      push!(opaque_factors, fac.args[1])
    else
      value = f(value, fac.args[1])
    end
  end

  if !isempty(opaque_factors)
    flattened_factors = Any[]
    for fac in opaque_factors
      if Meta.isexpr(fac, :call) && fac.args[1] === Symbol(f)
        append!(flattened_factors, fac.args[2:end])
      else
        push!(flattened_factors, fac)
      end
    end
    !isunit(value) && push!(flattened_factors, value)
    length(flattened_factors) == 1 && return factor(flattened_factors[1])

    ex = Expr(:call)
    ex.args = flattened_factors
    pushfirst!(ex.args, Symbol(f))
    return factor(ex)
  end
  return factor(value)
end

blade_complement(sig::Signature, b::Expression) = blade(sig, reverse(setdiff(1:dimension(sig), b.args)))

function expected_nargs(head)
  in(head, (:factor, :inverse, :reverse, :antireverse,)) && return 1
  in(head, (:project, :⋅, :/, :×, :⩒)) && return 2
  nothing
end

inverse(ex::Expr) = :(inv($ex))
inverse(ex) = inv(ex)

function infer_grade(head::Symbol, args)
  in(head, (:factor, ⦿)) && return 0
  head === :blade && return count(isodd, map(x -> count(==(x), args), unique(args)))
  if head === :⟑
    # Fast path for frequently encountered expressions.
    length(args) == 2 && isexpr(args[1], :factor) && return (args[2]::Expression).grade

    computed_grade = compute_grade_for_product(args)
    !isnothing(computed_grade) && return computed_grade
  end
  if in(head, (:+, :multivector))
    gs = Int64[]
    for arg in args
      append!(gs, grade(arg))
    end
    sort!(unique!(gs))
    length(gs) == 1 && return first(gs)
    return gs
  end
  if head === :kvector
    grades = map(args) do arg
      isa(arg, Expression) || error("Expected argument of type $Expression, got $arg")
      g = arg.grade
      isa(g, Int) || error("Expected grade to be known for argument $arg in $head expression")
      g
    end
    unique!(grades)
    length(grades) == 1 || error("Expected unique grade for k-vector expression, got grades $grades")
    return first(grades)
  end
  error("Cannot infer grade for unexpected $head expression with arguments $args")
end

function compute_grade_for_product(args)
  res = nothing
  for arg in args
    g = grade(arg)
    iszero(g) && continue
    if isnothing(res)
      res = g
      continue
    end
    if res ≠ g
      res = nothing
      break
    end
  end
  res
end

# Basic interfaces.

Base.:(==)(x::Expression, y::Expression) = x.head == y.head && (!isdefined(x, :grade) || !isdefined(y, :grade) || x.grade == y.grade) && x.args == y.args
Base.getindex(x::Expression, args...) = x.args[args...]
Base.firstindex(x::Expression) = firstindex(x.args)
Base.lastindex(x::Expression) = lastindex(x.args)
Base.length(x::Expression) = length(x.args)
Base.iterate(x::Expression, args...) = iterate(x.args, args...)
Base.keys(x::Expression) = keys(x.args)
Base.view(x::Expression, args...) = view(x.args, args...)

# Introspection utilities.

isexpr(ex, head::Symbol) = isa(ex, Expression) && ex.head === head
isexpr(ex, heads) = isa(ex, Expression) && in(ex.head, heads)
isexpr(ex, head::Symbol, n::Int) = isa(ex, Expression) && isexpr(ex, head) && length(ex.args) == n
isexpr(heads) = ex -> isexpr(ex, heads)
grade(ex::Expression) = ex.grade::GradeInfo
grades(ex::Expression) = ex.grade::Vector{Int}
isblade(ex, n = nothing) = isexpr(ex, :blade) && (isnothing(n) || n == length(ex))
isscalar(ex::Expression) = isblade(ex, 0) || isweightedblade(ex, 0)
isweighted(ex) = isexpr(ex, :⟑, 2) && isexpr(ex[1]::Expression, :factor)
isweightedblade(ex, n = nothing) = isweighted(ex) && isblade(ex[2]::Expression, n)
isopaquefactor(ex) = isexpr(ex, :factor) && isa(ex[1], Union{Symbol, Expr, QuoteNode})

function basis_vectors(ex::Expression)
  isweightedblade(ex) && return basis_vectors(ex[2]::Expression)
  isexpr(ex, :blade) && return collect(Int, ex.args)
  error("Expected blade or weighted blade expression, got $ex")
end

function lt_basis_order(xinds, yinds)
  nx, ny = length(xinds), length(yinds)
  nx ≠ ny && return nx < ny
  for (xi, yi) in zip(xinds, yinds)
    xi ≠ yi && return xi < yi
  end
  false
end

# Helper functions.

factor(x) = Expression(:factor, x)
weighted(x, fac) = Expression(:⟑, factor(fac), x)
scalar(fac) = factor(fac) * blade()
blade(xs...) = Expression(:blade, xs...)
kvector(args::AbstractVector) = Expression(:kvector, args)
kvector(xs...) = kvector(collect(Any, xs))
multivector(args::AbstractVector) = Expression(:multivector, args)
multivector(xs...) = multivector(collect(Any, xs))
project(g::GradeInfo, ex) = Expression(:project, g, ex)
Base.reverse(ex::Expression) = Expression(:reverse, ex)
antireverse(sig::Signature, ex::Expression) = simplified(sig, :antireverse, ex)
antiscalar(sig::Signature, fac) = factor(fac) * antiscalar(sig)
antiscalar(sig::Signature) = blade(1:dimension(sig))

blade(sig::Optional{Signature}, xs...) = simplified(sig, :blade, xs...)

⟑(x::Expression, args::Expression...) = Expression(:⟑, x, args...)
⟑(x, ys...) = *(x, ys...)
Base.:(*)(x::Expression, args::Expression...) = Expression(:*, x, args...)
Base.:(+)(x::Expression, args::Expression...) = Expression(:+, x, args...)
Base.:(-)(x::Expression, args::Expression...) = Expression(:-, x, args...)
Base.:(/)(x::Expression, y::Expression) = Expression(:/, x, y)
Base.inv(x::Expression) = Expression(:inverse, x)
⦿(x::Expression, y::Expression) = Expression(:⦿, x, y)
⋅(x::Expression, y::Expression) = Expression(:⋅, x, y)
∧(x::Expression, y::Expression) = Expression(:∧, x, y)
×(x::Expression, y::Expression) = Expression(:×, x, y)
exterior_product(x::Expression, y::Expression) = Expression(:∧, x, y)
exterior_product(sig::Signature, x::Expression, y::Expression) = simplified(sig, :∧, x, y)
exterior_antiproduct(sig::Signature, x::Expression, y::Expression) = simplified(sig, :∨, x, y)

# Traversal/transformation utilities.

walk(ex::Expression, inner, outer) = outer(Expression(ex.head, filter!(!isnothing, inner.(ex.args))))
walk(ex, inner, outer) = outer(ex)
postwalk(f, ex) = walk(ex, x -> postwalk(f, x), f)
prewalk(f, ex) = walk(f(ex), x -> prewalk(f, x), identity)

function traverse(f, ex, ::Type{T} = Expression) where {T}
  f(ex) === false && return nothing
  !isa(ex, T) && return nothing
  for arg in ex.args
    traverse(f, arg, T)
  end
end

# Display.

function Base.show(io::IO, ex::Expression)
  isexpr(ex, :blade) && return print(io, 'e', join(subscript.(ex)))
  isexpr(ex, :factor) && return print_factor(io, ex[1])
  if isexpr(ex, :⟑) && isexpr(ex[end], :blade)
    if length(ex) == 2 && isexpr(ex[1], :factor) && (!Meta.isexpr(ex[1][1], :call) || ex[1][1].args[1] == getcomponent)
      return print(io, ex[1], " ⟑ ", ex[end])
    else
      return print(io, '(', join(ex[1:(end - 1)], " ⟑ "), ") ⟑ ", ex[end])
    end
  end
  isexpr(ex, (:⟑, :+, :multivector)) && return print(io, '(', Expr(:call, ex.head, ex.args...), ')')
  if isexpr(ex, :project)
    printstyled(io, '⟨'; color = :yellow)
    print(io, ex[2])
    printstyled(io, '⟩', subscript(grade(ex)); color = :yellow)
    return
  end
  isexpr(ex, :kvector) && return print(io, Expr(:call, Symbol("kvector", subscript(ex.grade)), ex.args...))
  print(io, Expression, "(:", ex.head, ", ", join(ex.args, ", "), ')')
end

function print_factor(io, ex)
  Meta.isexpr(ex, :call) && in(ex.args[1], (:*, :+)) && return print(io, join((sprint(print_factor, arg) for arg in @view ex.args[2:end]), " $(ex.args[1]) "))
  Meta.isexpr(ex, :call, 3) && ex.args[1] === getcomponent && return print(io, Expr(:ref, ex.args[2:3]...))
  Meta.isexpr(ex, :call, 2) && ex.args[1] === getcomponent && return print(io, ex.args[2])
  print(io, ex)
end

"""
Return `val` as a subscript, used for printing `UnitBlade` and `Blade` instances.
"""
function subscript(val)
  r = div(val, 10)
  subscript_char(x) = Char(8320 + x)
  r > 0 ? string(subscript_char(r), subscript_char(mod(val, 10))) : string(subscript_char(val))
end
