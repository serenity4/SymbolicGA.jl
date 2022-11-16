mutable struct Expression
  head::Symbol
  grade::Optional{Int}
  args::Vector{Any}
  Expression(head::Symbol, args...) = Expression(head, collect(Any, args))
  function Expression(head::Symbol, args::AbstractVector)
    head === :scalar && isexpr(args[1], :scalar) && return args[1]
    if in(head, (:*, :+))
      length(args) == 1 && return only(args)
      any(isexpr(head), args) && (args = disassociate1(args, head))
      ns = count(isexpr(:scalar), args)
      # Collapse all scalars into one at the front, or return a scalar expression if all arguments are scalars.
      if ns ≥ 2
        scalars = ns == length(args) ? args : filter(isexpr(:scalar), args)
        scalar = Expression(:scalar, Expr(:call, head, [arg[1] for arg in scalars]...))
        ns == length(args) && return scalar
        return Expression(head, scalar, filter!(!isexpr(:scalar), args)...)
      elseif ns == 1 && !isexpr(args[1]::Expression, :scalar)
        i = findfirst(isexpr(:scalar), args)
        sc = args[i]::Expression
        deleteat!(args, i)
        pushfirst!(args, sc)
        return Expression(head, args)
      elseif head === :*
        # Collapse all bases and blades into one blade.
        nb = count(isexpr((:basis, :blade)), args)
        if nb ≥ 2
          xs = nb == length(args) ? args : filter(isexpr((:basis, :blade)), args)
          blade_args = []
          for x in xs
            isexpr(x, :basis) ? push!(blade_args, x) : append!(blade_args, x.args)
          end
          ex = blade(blade_args)
          nb == length(args) && return ex
          return *(ex, filter!(!isexpr((:basis, :blade)), args)...)
        end
      end
    end

    grade = extract_grade(head, args)
    in(head, (:scalar, :basis, :blade, :kvector, :project)) && grade::Int
    @assert head === :blade || !isempty(args)
    if head === :scalar
      @assert length(args) == 1
    elseif head === :basis
      @assert length(args) == 1 && isa(args[1], Int)
    elseif head === :blade
      @assert all(isexpr(ex, :basis) for ex in args)
    end
    new(head, grade, args)
  end
end

function extract_grade(head::Symbol, args)
  head === :scalar && return 0
  head === :basis && return 1
  head === :blade && return count(isodd, map(x -> count(==(x), args), args))
  head === :multivector && return nothing
  head === :project && return args[1]::Int
  head === :* && length(args) == 2 && isexpr(args[1], :scalar) && return (args[2]::Expression).grade
  if head === :+
    g = nothing
    for arg in args
      g′ = grade(arg)
      isnothing(g′) && return nothing
      isnothing(g) && (g = g′)
      g ≠ g′ && return nothing
    end
    !isnothing(g) && return g
  end
  head === :* && return nothing
  grades = map(args) do arg
    isa(arg, Expression) || error("Expected argument of type $Expression, got $arg")
    g = grade(arg)
    isa(g, Int) || error("Expected grade to be known for expression $arg")
    g
  end
  if head === :kvector
    unique!(grades)
    length(grades) == 1 || error("Expected unique grade for k-vector expression, got grades $grades")
    return first(grades)
  end
end

# Basic interfaces.

(==)(x::Expression, y::Expression) = x.head == y.head && x.grade === y.grade && x.args == y.args
Base.getindex(x::Expression, args...) = x.args[args...]
Base.firstindex(x::Expression) = firstindex(x.args)
Base.lastindex(x::Expression) = lastindex(x.args)
Base.length(x::Expression) = length(x.args)
Base.iterate(x::Expression, args...) = iterate(x.args, args...)
Base.view(x::Expression, args...) = view(x.args, args...)

# Introspection utilities.

isexpr(ex, head::Symbol) = isa(ex, Expression) && ex.head === head
isexpr(ex, heads) = isa(ex, Expression) && in(ex.head, heads)
isexpr(ex, head::Symbol, n::Int) = isa(ex, Expression) && isexpr(ex, head) && length(ex.args) == n
isexpr(heads) = ex -> isexpr(ex, heads)
isgrade(ex::Expression, grade::Int) = ex.grade === grade
grade(ex::Expression) = ex.grade
isweighted(ex) = isexpr(ex, :*, 2) && isexpr(ex[1]::Expression, :scalar)

# Helper functions.

scalar(x) = Expression(:scalar, x)
basis(i::Integer) = Expression(:basis, i)
blade(indices::Integer...) = blade(indices)
blade(args::AbstractVector) = Expression(:blade, args)
blade(args::AbstractVector{<:Integer}) = blade(Any[basis(i) for i in args])
blade(xs) = blade(Any[isa(x, Int) ? basis(x) : x for x in xs])
kvector(args::AbstractVector) = Expression(:kvector, args)
kvector(xs...) = kvector(collect(Any, xs))
multivector(args::AbstractVector) = Expression(:multivector, args)
multivector(xs...) = multivector(collect(Any, xs))
project(g::Integer, args) = Expression(:project, g, args)

weighted(ex::Expression, weight) = scalar(weight) * ex

Base.:(*)(x::Expression, y::Expression) = Expression(:*, x, y)
Base.:(+)(x::Expression, y::Expression) = Expression(:+, x, y)

# Traversal/transformation utilities.

walk(ex::Expression, inner, outer) = outer(Expression(ex.head, filter!(!isnothing, inner.(ex.args))))
walk(ex, inner, outer) = outer(ex)
postwalk(f, ex) = walk(ex, x -> postwalk(f, x), f)
prewalk(f, ex) = walk(f(ex), x -> prewalk(f, x), identity)

function traverse(f, ex)
  f(ex) === false && return nothing
  !isa(ex, Expression) && return nothing
  for arg in ex.args
    traverse(f, arg)
  end
end

# Display.

function Base.show(io::IO, ex::Expression)
  isexpr(ex, :basis) && return print(io, 'b', subscript(ex[1]::Int))
  isexpr(ex, :blade) && return print(io, 'e', join(subscript(ex[1]::Int) for ex in ex.args))
  isexpr(ex, :scalar) && return print_scalar(io, ex[1])
  if isexpr(ex, :*) && isexpr(ex[end], :blade)
    if length(ex) == 2 && isexpr(ex[1], :scalar) && (!Meta.isexpr(ex[1][1], :call) || ex[1][1].args[1] == getcomponent)
      return print(io, ex[1], " * ", ex[end])
    else
      return print(io, '(', join(ex[1:(end - 1)], " * "), ") * ", ex[end])
    end
  end
  isexpr(ex, (:*, :+, :multivector)) && return print(io, '(', Expr(:call, ex.head, ex.args...), ')')
  isexpr(ex, :project) && return print(io, '⟨', ex[2], '⟩', subscript(ex[1]::Int))
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
