using SymbolicGA: traverse, Retraversal

function expression_nodes(f, x, ::Type{Expr})
  res = Expr[]
  traverse(x) do y
    f(y) === true && push!(res, y)
    nothing
  end
  res
end

function expression_nodes(x::Expression)
  res = Expression[]
  traverse(x; retraversal = Retraversal(x.cache, Expr)) do y
    isa(y, Expression) && push!(res, y)
    nothing
  end
  res
end
