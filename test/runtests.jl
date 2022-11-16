using LazyGeometricAlgebra, Test
using LazyGeometricAlgebra: Expression, isexpr, isgrade, postwalk, canonicalize_blades, apply_metric, restructure_sums, disassociate_kvectors, extract_weights, kvector_expression, weighted, extract_base_expression, simplify, distribute, isweighted, getcomponent, blade, scalar, kvector, multivector, fill_kvector_components, basis

ENV["JULIA_DEBUG"] = "LazyGeometricAlgebra"
ENV["JULIA_DEBUG"] = ""

@testset "LazyGeometricAlgebra.jl" begin
    include("signatures.jl")
    include("expressions.jl")
    include("passes.jl")
    include("macro.jl")
end;
