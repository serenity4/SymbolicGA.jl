using SymbolicGA, Test
using SymbolicGA: Expression, isexpr, postwalk, simplify!, simplified, isweighted, getcomponent, blade, factor, weighted, scalar, antiscalar, kvector, multivector, antigrade, antireverse, exterior_product, ⟑, ∧, Head, FACTOR, BLADE, KVECTOR, MULTIVECTOR, ADDITION, SUBTRACTION, NEGATION, REVERSE, ANTIREVERSE, LEFT_COMPLEMENT, RIGHT_COMPLEMENT, GEOMETRIC_PRODUCT, EXTERIOR_PRODUCT, INTERIOR_PRODUCT, COMMUTATOR_PRODUCT, INVERSE, EXPONENTIAL

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
