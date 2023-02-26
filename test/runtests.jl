using SymbolicGA, Test
using SymbolicGA: Expression, ExpressionCache, ExpressionSpec, isexpr, postwalk, simplify!, isweighted, getcomponent, blade, factor, weighted, scalar, antiscalar, kvector, multivector, antigrade, antireverse, exterior_product, ⟑, ∧, Head, COMPONENT, FACTOR, BLADE, KVECTOR, MULTIVECTOR, ADDITION, SUBTRACTION, NEGATION, REVERSE, ANTIREVERSE, LEFT_COMPLEMENT, RIGHT_COMPLEMENT, GEOMETRIC_PRODUCT, EXTERIOR_PRODUCT, INTERIOR_PRODUCT, COMMUTATOR_PRODUCT, INVERSE, EXPONENTIAL, Term

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
