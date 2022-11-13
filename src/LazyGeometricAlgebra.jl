module LazyGeometricAlgebra

using Combinatorics

const Optional{T} = Union{T,Nothing}

import Base: sum, +, -, *, /, ^, inv, reverse, ==, ≈, eltype, promote_rule, length, zero, iszero, fill, getindex, setindex!, convert, show

include("utils.jl")
include("signatures.jl")
# include("lazy.jl")
include("expressions.jl")
include("passes.jl")
include("macro.jl")

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
