@testset "Passes" begin
  sig = Signature(1, 1, 1)
  ex = Expression(:blade, Expression(:basis, 1), Expression(:basis, 2), Expression(:basis, 1))
  ex2 = canonicalize_blades(ex)
  @test ex2 == weighted(Expression(:blade, ex.args[[1, 3, 2]]), -1)
  ex3 = apply_metric(ex2, sig)
  @test ex3 == weighted(Expression(:basis, 2), -1)

  e12 = Expression(:blade, Expression(:basis, 1), Expression(:basis, 2))
  b3 = Expression(:basis, 3)
  e123 = Expression(:blade, Expression(:basis, 1), Expression(:basis, 2), Expression(:basis, 3))
  ex = Expression(:*, e12, b3)
  ex2 = simplify_blade_products(ex)
  @test ex2 == e123

  e13 = Expression(:blade, Expression(:basis, 1), Expression(:basis, 3))
  x, y = weighted(e13, 2.0), weighted(e12, 1.2)
  ex = Expression(:+, x, y)
  ex2 = substitute_sums(ex)
  @test ex2 == Expression(:kvector, x, y)

  x, y, z = weighted(e13, 2.0), weighted(e12, 1.4), weighted(e123, 1.2)
  ex = Expression(:+, x, y, z)
  ex2 = substitute_sums(ex)
  @test ex2 == Expression(:multivector, Expression(:kvector, x, y), z)
end;
