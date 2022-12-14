module SymbolicGA

using Combinatorics
using Graphs

const Optional{T} = Union{T,Nothing}

include("utils.jl")
include("signatures.jl")
include("expressions.jl")
include("passes.jl")
include("interface.jl")
include("macro.jl")
include("types.jl")

export Signature,
  @ga,
  KVector,
  Bivector,
  Trivector,
  Quadvector,
  grade,
  codegen_expression,
  VariableInfo,
  parse_variable_info,
  @arg

end
