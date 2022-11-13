using LazyGeometricAlgebra, Test
using LazyGeometricAlgebra: Expression, isexpr, isgrade, postwalk, canonicalize_blades, apply_metric, restructure_sums, disassociate_kvectors, extract_weights, blade_expressions, kvector_expression, weighted, extract_base_expression, simplify, distribute, isweighted

blade(indices::Integer...) = Expression(:blade, Any[Expression(:basis, i) for i in indices])

@testset "LazyGeometricAlgebra.jl" begin
    include("signatures.jl")
    include("expressions.jl")
    include("passes.jl")
    include("macro.jl")
end;
