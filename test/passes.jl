@testset "Passes" begin
  sig = Signature(1, 1, 1)
  ex = blade(1, 2, 1)
  ex2 = canonicalize_blades(ex)
  @test ex2 == weighted(blade(1, 1, 2), -1)
  ex3 = apply_metric(ex2, sig)
  @test ex3 == weighted(blade(2), -1)

  e12 = blade(1, 2)
  b3 = Expression(:basis, 3)
  e123 = blade(1, 2, 3)
  ex = Expression(:*, e12, b3)
  @test ex == e123

  e13 = blade(1, 3)
  x, y = weighted(e13, 2.0), weighted(e12, 1.2)
  ex = Expression(:+, x, y)
  ex2 = restructure_sums(ex)
  @test ex2 == Expression(:kvector, x, y)

  x, y, z = weighted(e13, 2.0), weighted(e12, 1.4), weighted(e123, 1.2)
  ex = Expression(:+, x, y, z)
  ex2 = restructure_sums(ex)
  @test ex2 == Expression(:multivector, Expression(:kvector, x, y), z)
  @test isa(repr(ex2), String)

  w, x, y, z = (weighted(blade(mod1(i, 3)), 1.0) for i in 1:4)
  ex = Expression(:*, Expression(:+, w, y), Expression(:+, x, z))
  ex2 = distribute(ex)
  @test ex2 == Expression(:+, Expression(:*, w, x), Expression(:*, w, z), Expression(:*, y, x), Expression(:*, y, z))
end;
