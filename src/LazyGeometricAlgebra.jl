module LazyGeometricAlgebra

using Combinatorics

import Base: sum, +, -, *, /, ^, inv, reverse, ==, ≈, eltype, promote_rule, length, zero, iszero, fill, getindex, setindex!, convert, show

include("signatures.jl")
include("lazy.jl")

export @ga,
    # algebra
    Signature,
    dimension,
    triplet,
    is_degenerate,
    metric,

    # algebra elements
    scalar,
    kvector,
    vector,
    bivector,
    trivector,
    quadvector,
    pseudoscalar,
    multivector,

    # operators
    ∧, ⋅, ⦿, ∨, ×,
    lcontract, rcontract,
    dual,
    grade_projection,
    reverse_sign,
    magnitude,
    magnitude2

end
