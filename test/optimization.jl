sig = Signature(3, 1)
cache = ExpressionCache(sig)
sc(x) = scalar(cache, x)
fac(x) = factor(cache, x)
bl(args...) = blade(cache, args...)
x, y, z = sc.([:x, :y, :z])
e1, e2, e3, e4 = blade.(cache, [1, 2, 3, 4])

expression(head, args...) = Expression(cache, head, args...)
add(x, ys...) = expression(SCALAR_ADDITION, x, ys...)
mul(x, ys...) = expression(SCALAR_PRODUCT, x, ys...)
uadd(x, ys...) = unsimplified_expression(cache, SCALAR_ADDITION, x, ys...)
umul(x, ys...) = unsimplified_expression(cache, SCALAR_PRODUCT, x, ys...)

is_binarized(ex) = all(==(2) ∘ length, gather_scalar_expressions(ex))

@testset "Optimization" begin
  a = add(:x, :y, :z)
  @test !may_reuse(a, ExpressionSpec(a))
  @test may_reuse(a, ExpressionSpec(add(:x, :y)))
  @test !may_reuse(a, ExpressionSpec(mul(:x, :y)))
  b = add(:x, :z)
  @test !may_reuse(b, ExpressionSpec(add(:x, :y)))
  @test !may_reuse(add(:x, :y, :z), ExpressionSpec(add(:x, :x)))

  ex = add(:x, :y)
  @test gather_scalar_expressions(ex) == [ex]
  ex = uadd(:x, uadd(:y, :z))
  @test gather_scalar_expressions(ex) == [ex, add(:y, :z)]
  ex1 = uadd(:y, :(f($(add(:a, :b)))))
  ex = uadd(:x, ex1)
  @test gather_scalar_expressions(ex) == [ex, ex1, add(:a, :b)]
  ex1 = uadd(:a, :b, :(g($(mul(:c, :d)))))
  ex2 = uadd(:y, :(f($ex1)))
  ex = uadd(:x, ex2)
  @test gather_scalar_expressions(ex) == [ex, ex2, ex1, mul(:c, :d)]
  ex1 = umul(:y, expression(SCALAR_DIVISION, add(:a, :b), mul(:c, :d)))
  ex = uadd(:x, ex1)
  @test gather_scalar_expressions(ex) == [ex, ex1, add(:a, :b), mul(:c, :d)]

  ex = uadd(a, b)
  iter = IterativeRefinement(ex)
  apply!(iter)
  @test ex == uadd(uadd(:y, b), b)
  @test iter.metrics.reused == 1
  @test iter.metrics.splits == 0
  @test is_binarized(ex)

  a = add(:x, :y, :z)
  ex = uadd(a, a, umul(b, uadd(a, a)))
  iter = IterativeRefinement(ex)
  apply!(iter)
  @test a == uadd(:y, uadd(:x, :z))
  @test ex == uadd(umul(b, uadd(a, a)), uadd(a, a))
  @test iter.metrics.reused == 2
  @test iter.metrics.splits == 0
  @test is_binarized(ex)

  abcd = uadd(:a, :b, :c, :d)
  expr = Expr(:call, :f, abcd)
  ex = uadd(:x, uadd(:a, :b), expr)
  iter = IterativeRefinement(ex)
  @test in(abcd, iter.expressions)
  @test haskey(iter.available, 4)
  apply!(iter)
  @test iter.metrics.splits ≤ 2
  @test is_binarized(ex)

  ex, _ = generate_expression(sig, :(x::1 ⟑ y::2); optimize = true, factorize = false)
  @test is_binarized(ex)

  ex, _ = generate_expression(Signature(3), quote
    Π = a::Vector ⟑ b::Vector
    Ω = exp((-(0.5α)::Scalar) ⟑ Π)
  end; optimize = false, factorize = false)
  exs = gather_scalar_expressions(ex)
  @test allunique(exs)
  @test !is_binarized(ex)
  optimize!(ex)
  new_exs = gather_scalar_expressions(ex)
  @test length(new_exs) > length(exs)
  @test all(in(new_exs), exs)
  # TODO: Get a consistent result; the issue probably stems from the non-deterministic sorting of terms in `simplify_addition`.
  @test_skip is_binarized(ex)
end;
