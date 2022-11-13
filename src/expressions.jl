mutable struct Expression
  head::Symbol
  grade::Optional{Int}
  args::Vector{Any}
  Expression(head::Symbol, args...) = Expression(head, collect(args))
  function Expression(head::Symbol, args::AbstractVector)
    grade = extract_grade(head, args)
    in(head, (:scalar, :basis, :blade, :kvector, :project)) && grade::Int
    @assert !isempty(args)
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

(==)(x::Expression, y::Expression) = x.head == y.head && x.grade === y.grade && x.args == y.args

function extract_grade(head::Symbol, args)
  head === :scalar && return 0
  head === :basis && return 1
  head === :blade && return count(isodd, map(x -> count(==(x), args), args))
  head === :multivector && return nothing
  head === :project && return args[1]::Int
  head === :* && length(args) == 2 && isexpr(args[1], :scalar) && return (args[2]::Expression).grade
  grades = [(arg::Expression).grade::Int for arg in args]
  if head === :kvector
    unique!(grades)
    length(grades) == 1 || error("Expected unique grade for k-vector expression, got grades $grades")
    return first(grades)
  end
end

isexpr(ex, head::Symbol) = isa(ex, Expression) && ex.head === head
isexpr(ex, heads) = isa(ex, Expression) && in(ex.head, heads)
isexpr(ex, head::Symbol, n::Int) = isa(ex, Expression) && isexpr(ex, head) && length(ex.args) == n
isgrade(ex::Expression, grade::Int) = ex.grade === grade
grade(ex::Expression) = ex.grade

weighted(ex::Expression, weight) = Expression(:*, Expression(:scalar, weight), ex)

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
