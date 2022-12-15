using LazyGeometricAlgebra, Test
using LazyGeometricAlgebra: Expression, isexpr, postwalk, simplify!, simplified, isweighted, getcomponent, blade, factor, weighted, scalar, antiscalar, kvector, multivector, project, grade, antigrade, antireverse, exterior_product, exterior_antiproduct, ⟑, ∧, KVector

ENV["JULIA_DEBUG"] = "LazyGeometricAlgebra"
ENV["JULIA_DEBUG"] = ""

@testset "LazyGeometricAlgebra.jl" begin
    include("signatures.jl")
    include("expressions.jl")
    include("passes.jl")
    include("types.jl")
    include("macro.jl")
    include("operators.jl")
    include("examples.jl")
end;
