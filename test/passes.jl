@testset "Passes" begin
  e12 = blade(1, 2)
  e13 = blade(1, 3)
  e4 = blade(4)
  e123 = blade(1, 2, 3)

  w, x, y, z = (weighted(blade(mod1(i, 3)), 1.0) for i in 1:4)
  ex = (w + y) * (x + z)
  ex2 = distribute(ex)
  @test ex2 == (w * x) + (w * z) + (y * x) + (y * z)

  ex = scalar(1.0) * kvector(e12)
  ex2 = distribute(ex)
  @test ex2 == weighted(e12, 1.0)

  ex = (kvector(weighted(e12, 1.0)) + kvector(weighted(e4, 3.0))) * kvector(weighted(e123, 1.0))
  ex2 = distribute(ex)
  @test ex2 == weighted(blade(1, 2, 1, 2, 3), scalar(1.0) * scalar(1.0)) + weighted(blade(4, 1, 2, 3), scalar(1.0) * scalar(3.0))

  x, y = weighted(e13, 2.0), weighted(e12, 1.2)
  ex2 = restructure_sums(x + y)
  @test ex2 == kvector(x, y)

  x, y, z = weighted(e13, 2.0), weighted(e12, 1.4), weighted(e123, 1.2)
  ex2 = restructure_sums(x + y + z)
  @test ex2 == multivector(kvector(x, y), z)
  @test isa(repr(ex2), String)

  sig = Signature(1, 1, 1)
  ex = blade(1, 2, 1)
  ex2 = canonicalize_blades(ex)
  @test ex2 == weighted(blade(1, 1, 2), -1)
  ex3 = apply_metric(ex2, sig)
  @test ex3 == weighted(blade(2), -1)

  ex = kvector(blade(1, 2), blade(2, 3))
  ex2 = fill_kvector_components(ex, sig)
  @test ex2 == kvector(blade(1, 2), weighted(blade(1, 3), 0), blade(2, 3))
end;
