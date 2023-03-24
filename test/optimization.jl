using SymbolicGA, Test

sig = Signature(3, 1)
cache = ExpressionCache(sig)

@testset "Optimization" begin
  ex = Expression(cache, SCALAR_ADDITION, :x, :y, :z)
  spec = ExpressionSpec(cache, SCALAR_ADDITION, ex[1], ex[2])
  @test may_reuse(spec, ex)
  ex2 = uncached_expression(cache, SCALAR_ADDITION, :a, ex)
  @test may_reuse(spec, ex2)
  ex3 = Expression(cache, SCALAR_ADDITION, :x, :z)
  @test !may_reuse(spec, ex3)
end;
