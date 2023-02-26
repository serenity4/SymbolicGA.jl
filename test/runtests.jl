using SymbolicGA, Test
using SymbolicGA: Expression, ExpressionCache, ExpressionSpec, isexpr, postwalk, simplify!, isweighted, getcomponent, blade, weighted, scalar, antiscalar, kvector, multivector, antigrade, antireverse, exterior_product, ⟑, ∧, Head, SCALAR, BLADE, KVECTOR, MULTIVECTOR, ADDITION, SUBTRACTION, NEGATION, REVERSE, ANTIREVERSE, LEFT_COMPLEMENT, RIGHT_COMPLEMENT, GEOMETRIC_PRODUCT, EXTERIOR_PRODUCT, INTERIOR_PRODUCT, COMMUTATOR_PRODUCT, INVERSE, EXPONENTIAL, SCALAR_ADDITION, SCALAR_EXPONENTIAL, SCALAR_PRODUCT, SCALAR_DIVISION, SCALAR_NAN_TO_ZERO, SCALAR_INVERSE, SCALAR_COS, SCALAR_SIN, SCALAR_COSH, SCALAR_SINH, SCALAR_SQRT, SCALAR_ABS, SCALAR_COMPONENT, SCALAR_NEGATION, SCALAR_SUBTRACTION

ENV["JULIA_DEBUG"] = "SymbolicGA"
ENV["JULIA_DEBUG"] = ""

@testset "SymbolicGA.jl" begin
    include("signatures.jl")
    include("expressions.jl")
    include("passes.jl")
    include("types.jl")
    include("macro.jl")
    include("operators.jl")
    include("examples.jl")
    include("doctests.jl")
end;
