using SymbolicGA, Test

sig = Signature(3, 1)
cache = ExpressionCache(sig)

add(x, ys...; cache = x.cache) = Expression(cache, SCALAR_ADDITION, x, ys...)
mul(x, ys...; cache = x.cache) = Expression(cache, SCALAR_PRODUCT, x, ys...)
uncached_add(x, ys...; cache = x.cache) = uncached_expression(cache, SCALAR_ADDITION, x, ys...)
uncached_mul(x, ys...; cache = x.cache) = uncached_expression(cache, SCALAR_PRODUCT, x, ys...)

@testset "Optimization" begin
  a = add(:x, :y, :z; cache)
  @test !may_reuse(a, a)
  @test may_reuse(a, add(:x, :y; cache))
  @test !may_reuse(a, mul(:x, :y; cache))
  b = add(:x, :z; cache)
  @test !may_reuse(b, add(:x, :y; cache))

  ex = uncached_add(a, b)
  iter = IterativeRefinement(ex)
  apply!(iter)
  @test ex == uncached_add(uncached_add(:y, b; cache), b)
  @test iter.metrics.reused == 1

  abcd = uncached_add(:a, :b, :c, :d; cache)
  expr = Expr(:call, :f, abcd)
  nested = uncached_add(:x, uncached_add(:a, :b; cache), expr; cache)
  iter = IterativeRefinement(nested)
  @test in(abcd, iter.expressions)
  @test haskey(iter.available, 4)

  a = add(:x, :y, :z; cache)
  ex = uncached_add(a, a, uncached_mul(b, uncached_add(a, a)))
  iter = IterativeRefinement(ex)
  apply!(iter)
  @test a == uncached_add(:y, uncached_add(:x, :z; cache); cache)
  @test ex == uncached_add(uncached_mul(b, uncached_add(a, a)), uncached_add(a, a))
  @test iter.metrics.reused == 2
end;
