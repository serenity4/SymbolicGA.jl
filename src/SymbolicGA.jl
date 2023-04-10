module SymbolicGA

using Combinatorics
using Graphs
using CompileTraces

const Optional{T} = Union{T,Nothing}

include("utils.jl")
include("signatures.jl")
include("expressions.jl")
include("passes.jl")
include("interface.jl")
include("macro.jl")
include("types.jl")
include("optimization.jl")
include("factorization.jl")
@compile_traces verbose = false joinpath(@__DIR__, "precompilation_traces.jl")

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
