module SymbolicGA

using Combinatorics
using Graphs
using CompileTraces: @compile_traces
using PrecompileTools: @compile_workload
using Dictionaries

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

@compile_workload @compile_traces "precompilation_traces.jl"

export Signature,
  @ga,
  KVector,
  Bivector,
  Trivector,
  Quadvector,
  grade,
  codegen_expression,
  Bindings,
  builtin_bindings,
  parse_bindings,
  @arg

end
