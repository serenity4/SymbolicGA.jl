using LazyGeometricAlgebra, Test
using LazyGeometricAlgebra: Expression, isexpr, isgrade, postwalk, canonicalize_blades, apply_metric, simplify_blade_products, substitute_sums, disassociate_kvectors, extract_weights, blade_expressions, kvector_expression, weighted

@testset "LazyGeometricAlgebra.jl" begin
    include("signatures.jl")
    include("expressions.jl")
    include("passes.jl")
    include("macro.jl")
    # include("lazy.jl")
end;
