module SymbolicGA

using Combinatorics
using Graphs
using CompileTraces: @compile_traces
using PrecompileTools: @compile_workload
using Dictionaries
using Logging: with_logger, NullLogger

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
include("spaces.jl")

# @compile_workload @compile_traces "precompilation_traces.jl"

export Signature,
  @ga,
  @geometric_space,
  @pga2, @pga3, @cga3,
  KVector,
  Bivector,
  Trivector,
  Quadvector,
  grade,
  codegen_expression,
  Bindings,
  default_bindings,
  parse_bindings,
  @arg

end
