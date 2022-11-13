mutable struct Expression
  head::Symbol
  grade::Optional{Int}
  args::Vector{Any}
  Expression(head::Symbol, args...) = Expression(head, collect(args))
  function Expression(head::Symbol, args::Vector)
    grade = extract_grade(head, args)
    head !== :multivector && grade::Int
    new(head, grade, args)
  end
end

(==)(x::Expression, y::Expression) = x.head == y.head && x.grade === y.grade && x.args == y.args

function extract_grade(head::Symbol, args)
  head === :scalar && return 0
  head === :basis && return 1
  head === :blade && return count(isodd, map(x -> count(==(x), args), args))
  head === :multivector && return nothing
  grades = [(arg::Expression).grade::Int for arg in args]
  if head === :kvector
    unique!(grades)
    length(grades) == 1 || error("Expected unique grade for k-vector expression, got grades $grades")
    return first(grades)
  end
end

isexpr(ex::Expression, head::Symbol) = ex.head === head
isexpr(ex::Expression, heads) = in(ex.head, heads)
isexpr(ex::Expression, head::Symbol, n::Integer) = isexpr(ex, head) && length(ex.args) == n
isgrade(ex::Expression, grade::Integer) = ex.grade === grade

walk(ex::Expression, inner, outer) = outer(Expression(ex.head, filter!(!isnothing, inner.(ex.args))))
postwalk(f, ex::Expression) = walk(ex, x -> postwalk(f, x), f)
prewalk(f, ex::Expression) = walk(f(ex), x -> prewalk(f, x), identity)
