using LazyGeometricAlgebra, Test
using LazyGeometricAlgebra: Expression, isexpr, isgrade, postwalk, weighted, simplify, isweighted, getcomponent, blade, scalar, kvector, multivector, basis

ENV["JULIA_DEBUG"] = "LazyGeometricAlgebra"
ENV["JULIA_DEBUG"] = ""

@testset "LazyGeometricAlgebra.jl" begin
    include("signatures.jl")
    include("expressions.jl")
    include("passes.jl")
    include("macro.jl")
end;
