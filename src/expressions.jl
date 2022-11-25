const GradeInfo = Union{Int,Vector{Int}}

mutable struct Expression
  head::Symbol
  grade::GradeInfo
  args::Vector{Any}
  function Expression(head::Symbol, args::AbstractVector; simplify = true, grade = nothing)
    ex = new()
    ex.head = head
    ex.args = args
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
  # Simplify nested scalar expressions.
  head === :scalar && isexpr(args[1], :scalar) && return simplify!(args[1]::Expression, sig)
  # Force inverse of scalar to be a scalar expression at top-level.
  if head === :inverse && isexpr(args[1], :scalar)
    arg = inverse(args[1][1])
    return simplified(sig, :scalar, arg)
  end
  # Simplify scalar reverses.
  head === :reverse && isexpr(args[1], :scalar) && return simplify!(args[1], sig)

  if head === :blade
    # Sort basis vectors.
    if !issorted(args)
      perm = sortperm(args)
      fac = isodd(parity(perm)) ? -1 : 1
      return simplified(sig, :*, simplified(sig, :scalar, fac), simplified(sig, :blade, args[perm]))
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
            if iszero(m)
              fac = 0
              break
            end
            fac *= m
            length(args) == 2 && return simplified(sig, :scalar, fac)
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
      iszero(fac) && return simplified(sig, :scalar, fac)
      return simplified(sig, :*, simplified(sig, :scalar, fac), simplified(sig, :blade, args))
    end
  end

  if head === :-
    # Simplify unary minus operator to a multiplication with -1.
    length(args) == 1 && return simplified(sig, :*, scalar(-1), args[1])
    # Transform binary minus expression into an addition.
    @assert length(args) == 2
    return simplified(sig, :+, args[1], -args[2])
  end

  # Simplify whole expression to zero if a product term is zero.
  head === :* && any(isexpr(x, :scalar) && x[1] == 0 for x in args) && return scalar(0)

  # Remove scalar unit elements for multiplication and addition.
  if head === :*
    filter!(x -> !isexpr(x, :scalar) || x[1] ≠ 1, args)
    isempty(args) && return scalar(1)
  elseif head === :+
    filter!(x -> !isexpr(x, :scalar) || x[1] ≠ 0, args)
    isempty(args) && return scalar(0)
  end

  if in(head, (:*, :+))
    length(args) == 1 && return args[1]

    # Disassociate * and +.
    any(isexpr(head), args) && return disassociate1(args, head, sig)

    # Apply scalar simplifications and group any scalars at the front.
    ns = count(isexpr(:scalar), args)
    if ns ≥ 2
      # Collapse all scalars into one at the front, or return a scalar expression if all arguments are scalars.
      scalars, nonscalars = filter(isexpr(:scalar), args), filter(!isexpr(:scalar), args)
      opaque_scalars = filter(isopaquescalar, scalars)
      if length(opaque_scalars) ≥ 2
        opaque_part = scalar(Expr(:call, head, [arg[1] for arg in opaque_scalars]...))
        opaque_scalar = length(opaque_scalars) == ns ? opaque_part : Expression(head, scalar.(filter!(!isopaquescalar, scalars))..., opaque_part)
        length(opaque_scalars) == length(args) && return opaque_scalar
        scalars = opaque_scalar
      end
      args = [scalars; nonscalars]
    elseif ns == 1 && !isexpr(args[1]::Expression, :scalar)
      # Put the scalar at the front.
      i = findfirst(isexpr(:scalar), args)
      sc = args[i]::Expression
      deleteat!(args, i)
      pushfirst!(args, sc)
      return Expression(head, args)
    end

    # Collapse all blades as one.
    if head === :*
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
        return simplified(sig, :*, Any[ex; args])
      end
    end
  end

  # Check that arguments make sense.
  n = expected_nargs(head)
  !isnothing(n) && @assert length(args) == n "Expected $n arguments for expression $head, $n were provided\nArguments: $args"
  @assert head === :blade || !isempty(args)
  head === :blade && @assert all(isa(i, Int) for i in args)
  if head === :project
    @assert isa(args[1], Int) && isa(args[2], Expression)
    !isnothing(sig) && @assert 0 ≤ args[1] ≤ dimension(sig)
  end
  head === :dual && @assert isa(args[1], Expression)


  # Distribute products over addition.
  head in (:*, :⦿, :∧, :⋅, :×) && any(isexpr(arg, :+) for arg in args) && return distribute1(ex, head, sig)

  # Eagerly apply projections.
  head === :project && return project!(args[2]::Expression, args[1]::GradeInfo)

  # Eagerly apply reversions.
  head === :reverse && return apply_reverse_operators(ex)

  # Expand common operators.
  # ========================

  # The outer product is associative, no issues there.
  head === :∧ && return project!(Expression(:*, ex.args), sum(grade, ex))

  if head === :⋅
    # The inner product must have only two operands, as no associativity law is available to derive a canonical binarization.
    # Homogeneous vectors are expected, so the grade should be known.
    r, s = grade(args[1]::Expression)::Int, grade(args[2]::Expression)::Int
    (iszero(r) || iszero(s)) && return scalar(0)
    return project!(simplified(sig, :*, args), abs(r - s))
  end

  head === :⦿ && return project!(simplified(sig, :*, args), 0)

  if head === :×
    # The commutator product must have only two operands, as no associativity law is available to derive a canonical binarization.
    a, b = args[1]::Expression, args[2]::Expression
    return simplified(sig, :*, scalar(0.5), simplified(sig, :*, a, b) - simplified(sig, :*, b, a))
  end

  if head === :inverse
    a = args[1]::Expression
    return reverse(a) / simplified(sig, :⦿, a, a)
  end

  if head === :/
    a, b = args[1]::Expression, args[2]::Expression
    return simplified(sig, :*, a, simplified(sig, :inverse, b))
  end

  head === :dual && return simplified(sig, :*, args[1]::Expression, reverse(pseudoscalar(sig)))

  ex.grade = infer_grade(head, args)::GradeInfo
  ex.args = args

  ex
end

function expected_nargs(head)
  in(head, (:scalar, :inverse, :reverse, :dual)) && return 1
  in(head, (:project, :⋅, :/, :×)) && return 2
  nothing
end

inverse(ex::Expr) = :(inv($ex))
inverse(ex) = inv(ex)

function infer_grade(head::Symbol, args)
  in(head, (:scalar, ⦿)) && return 0
  head === :blade && return count(isodd, map(x -> count(==(x), args), unique(args)))
  if head === :*
    # Fast path for frequently encountered expressions.
    length(args) == 2 && isexpr(args[1], :scalar) && return (args[2]::Expression).grade

    computed_grade = compute_grade_for_product(args)
    !isnothing(computed_grade) && return computed_grade
  end
  in(head, (:+, :multivector)) && return sort!(unique!(grade.(args)))
  if head === :kvector
    grades = map(filter(≠(scalar(0)), args)) do arg
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

function compute_grade_for_sum(args)
  g = nothing
  for arg in args
    g′ = arg.grade
    isnothing(g′) && return nothing
    isnothing(g) && (g = g′)
    g ≠ g′ && return nothing
  end
  g
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
isweighted(ex) = isexpr(ex, :*, 2) && isexpr(ex[1]::Expression, :scalar)
isweightedblade(ex) = isweighted(ex) && isexpr(ex[2]::Expression, :blade)
isopaquescalar(ex) = isexpr(ex, :scalar) && !isa(ex[1], Expression)

function basis_vectors(ex::Expression)
  isweightedblade(ex) && return basis_vectors(ex[2]::Expression)
  isexpr(ex, :blade) && return collect(Int, ex.args)
  isexpr(ex, :scalar) && return Int[]
  error("Expected scalar, blade or weighted blade expression, got $ex")
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

scalar(x) = Expression(:scalar, x)
blade(xs...) = Expression(:blade, xs...)
kvector(args::AbstractVector) = Expression(:kvector, args)
kvector(xs...) = kvector(collect(Any, xs))
multivector(args::AbstractVector) = Expression(:multivector, args)
multivector(xs...) = multivector(collect(Any, xs))
project(g::GradeInfo, ex) = Expression(:project, g, ex)
Base.reverse(ex::Expression) = Expression(:reverse, ex)
pseudoscalar(s::Signature) = blade(1:dimension(s))

blade(sig::Optional{Signature}, xs...) = simplified(sig, :blade, xs...)

Base.:(*)(x::Expression, args::Expression...) = Expression(:*, x, args...)
Base.:(+)(x::Expression, args::Expression...) = Expression(:+, x, args...)
Base.:(-)(x::Expression, args::Expression...) = Expression(:-, x, args...)
Base.:(/)(x::Expression, y::Expression) = Expression(:/, x, y)
Base.inv(x::Expression) = Expression(:inverse, x)
⦿(x::Expression, y::Expression) = Expression(:⦿, x, y)
⋅(x::Expression, y::Expression) = Expression(:⋅, x, y)
∧(x::Expression, y::Expression) = Expression(:∧, x, y)
×(x::Expression, y::Expression) = Expression(:×, x, y)
∨(x::Expression, y::Expression) = Expression(:∨, x, y)

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
  isexpr(ex, :scalar) && return print_scalar(io, ex[1])
  if isexpr(ex, :*) && isexpr(ex[end], :blade)
    if length(ex) == 2 && isexpr(ex[1], :scalar) && (!Meta.isexpr(ex[1][1], :call) || ex[1][1].args[1] == getcomponent)
      return print(io, ex[1], " * ", ex[end])
    else
      return print(io, '(', join(ex[1:(end - 1)], " * "), ") * ", ex[end])
    end
  end
  isexpr(ex, (:*, :+, :multivector)) && return print(io, '(', Expr(:call, ex.head, ex.args...), ')')
  if isexpr(ex, :project)
    printstyled(io, '⟨'; color = :yellow)
    print(io, ex[2])
    printstyled(io, '⟩', subscript(grade(ex)); color = :yellow)
    return
  end
  isexpr(ex, :kvector) && return print(io, Expr(:call, Symbol("kvector", subscript(ex.grade)), ex.args...))
  print(io, Expression, "(:", ex.head, ", ", join(ex.args, ", "), ')')
end

function print_scalar(io, ex)
  Meta.isexpr(ex, :call) && in(ex.args[1], (:*, :+)) && return print(io, join((sprint(print_scalar, arg) for arg in @view ex.args[2:end]), " $(ex.args[1]) "))
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
