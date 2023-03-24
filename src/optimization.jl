struct IterativeRefinement
  available::Set{ExpressionSpec}
  expressions::Vector{Expression}
end

ismatch(spec::ExpressionSpec, ex::Expression) = spec.head == ex.head && spec.args == ex.args

using AbstractTrees: AbstractTrees, PreOrderDFS

AbstractTrees.children(ex::Expression) = ex.args
AbstractTrees.children(::ID) = ()

function may_reuse(spec::ExpressionSpec, ex::Expression)
  for ex in PreOrderDFS(ex)
    isexpr(ex, spec.head) && all(in(ex), spec.args) && return true
  end
  false
end

function apply!(iter::IterativeRefinement)
end
