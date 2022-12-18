using SymbolicGA
using BenchmarkTools

x = (1.0, 2.0, 3.0)
y = (50.0, 70.0, 70.0)

# Constant propagation does all the work.
@btime @ga 3 $x::Vector ⟑ $y::Vector

function f(x, y)
  @ga 3 Tuple x::Vector ⟑ y::Vector
end

@btime f(x, y)
@code_typed f(x, y)
