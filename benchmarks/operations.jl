using SymbolicGA
using BenchmarkTools

x = (1.0, 2.0, 3.0)
y = (50.0, 70.0, 70.0)
@btime @ga 3 $x::Vector * $y::Vector

function f(x, y)
  @ga 3 x::Vector * y::Vector
end

@code_typed f(x, y)
