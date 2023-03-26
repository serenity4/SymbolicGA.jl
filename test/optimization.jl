using SymbolicGA, Test

sig = Signature(3, 1)
cache = ExpressionCache(sig)

add(x, ys...; cache = x.cache) = Expression(cache, SCALAR_ADDITION, x, ys...)
mul(x, ys...; cache = x.cache) = Expression(cache, SCALAR_PRODUCT, x, ys...)
unsimplified_add(x, ys...; cache = x.cache) = unsimplified_expression(cache, SCALAR_ADDITION, x, ys...)
unsimplified_mul(x, ys...; cache = x.cache) = unsimplified_expression(cache, SCALAR_PRODUCT, x, ys...)

@testset "Optimization" begin
  a = add(:x, :y, :z; cache)
  @test !may_reuse(a, a)
  @test may_reuse(a, add(:x, :y; cache))
  @test !may_reuse(a, mul(:x, :y; cache))
  b = add(:x, :z; cache)
  @test !may_reuse(b, add(:x, :y; cache))

  ex = unsimplified_add(a, b)
  iter = IterativeRefinement(ex)
  apply!(iter)
  @test ex == unsimplified_add(unsimplified_add(:y, b; cache), b)
  @test iter.metrics.reused == 1
  @test iter.metrics.splits == 0

  a = add(:x, :y, :z; cache)
  ex = unsimplified_add(a, a, unsimplified_mul(b, unsimplified_add(a, a)))
  iter = IterativeRefinement(ex)
  apply!(iter)
  @test a == unsimplified_add(:y, unsimplified_add(:x, :z; cache); cache)
  @test ex == unsimplified_add(unsimplified_mul(b, unsimplified_add(a, a)), unsimplified_add(a, a))
  @test iter.metrics.reused == 2
  @test iter.metrics.splits == 0

  abcd = unsimplified_add(:a, :b, :c, :d; cache)
  expr = Expr(:call, :f, abcd)
  nested = unsimplified_add(:x, unsimplified_add(:a, :b; cache), expr; cache)
  iter = IterativeRefinement(nested)
  @test in(abcd, iter.expressions)
  @test haskey(iter.available, 4)
  apply!(iter)
  @test iter.metrics.splits ≤ 2
  @test all(==(2) ∘ length, expression_nodes(ex))
end;
