module LazyGeometricAlgebra

using Combinatorics

const Optional{T} = Union{T,Nothing}

include("utils.jl")
include("signatures.jl")
include("expressions.jl")
include("passes.jl")
include("interface.jl")
include("macro.jl")
# include("types.jl")

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
