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

@testset "Factorization" begin
  a, b, c, d, e, f = (:a, :b, :c, :d, :e, :f)
  ex = add(mul(a, b), mul(a, c))
  fact = Factorization(ex)
  @test apply!(fact) ≈ umul(a, uadd(b, c))

  ex = add(mul(a, c), mul(a, d), mul(b, c), mul(b, d), e)
  @test factorize(ex) ≈ uadd(umul(add(a, b), add(c, d)), e)

  ex = add(mul(a, c, e), mul(a, c, f), mul(a, d, e), mul(a, d, f), mul(b, c, e), mul(b, c, f), mul(b, d, e), mul(b, d, f))
  @test factorize(ex) ≈ umul(add(a, b), add(c, d), add(e, f))

  ex = add(mul(a, b, c), mul(c, d, e), mul(a, d, f))
  @test factorize(ex) in (
    uadd(umul(a, add(mul(b, c), mul(d, f))), mul(c, d, e)),
    uadd(umul(c, uadd(mul(a, b), mul(d, e))), mul(a, d, f)),
    uadd(umul(d, add(mul(c, e), mul(a, f))), mul(a, b, c)),
  )
end;
