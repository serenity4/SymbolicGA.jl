using SymbolicGA, Test
using SymbolicGA: Term, Expression, ID, ExpressionCache, ExpressionSpec, isexpr, postwalk, simplify!, isweighted, getcomponent, blade, factor, weighted, scalar, antiscalar, kvector, multivector, antigrade, antireverse, exterior_product, ⟑, ∧, Head, COMPONENT, FACTOR, BLADE, KVECTOR, MULTIVECTOR, ADDITION, SUBTRACTION, NEGATION, REVERSE, ANTIREVERSE, LEFT_COMPLEMENT, RIGHT_COMPLEMENT, GEOMETRIC_PRODUCT, EXTERIOR_PRODUCT, INTERIOR_PRODUCT, COMMUTATOR_PRODUCT, INVERSE, EXPONENTIAL, SCALAR_ADDITION, SCALAR_PRODUCT, SCALAR_DIVISION, may_reuse, unsimplified_expression, IterativeRefinement, apply!, optimize!, dereference, gather_scalar_expressions, generate_expression, show1, Factorization, factorize, factorize!, Scalar, dimension

ENV["JULIA_DEBUG"] = "SymbolicGA"
ENV["JULIA_DEBUG"] = ""

include("utils.jl")

@testset "SymbolicGA.jl" begin
    include("signatures.jl")
    include("expressions.jl")
    include("factorization.jl")
    include("optimization.jl")
    include("passes.jl")
    include("types.jl")
    include("macro.jl")
    include("operators.jl")
    include("examples.jl")
    include("doctests.jl")
end;
