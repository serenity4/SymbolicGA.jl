using LazyGeometricAlgebra: distribute, restructure_sums, canonicalize_blades, apply_metric, disassociate_kvectors, fill_kvector_components, apply_reverse_operators

@testset "Passes" begin
  e12 = blade(1, 2)
  e13 = blade(1, 3)
  e4 = blade(4)
  e123 = blade(1, 2, 3)

  x, y = weighted(e13, 2.0), weighted(e12, 1.2)
  ex = restructure_sums(x + y)
  @test ex == kvector(x, y)

  z = weighted(e123, 1.2)
  ex = restructure_sums(x + y + z)
  @test ex == multivector(kvector(x, y), z)
  @test isa(repr(ex), String)

  ex = apply_reverse_operators(reverse(blade(1, 2)))
  @test ex == blade(2, 1)
  ex = apply_reverse_operators(reverse(weighted(blade(1, 2), 2.0)))
  @test ex == weighted(blade(2, 1), 2.0)
  ex = apply_reverse_operators(reverse(weighted(blade(1, 2), 2.0) * kvector(blade(1), blade(2))))
  @test ex == kvector(blade(1), blade(2)) * weighted(blade(2, 1), 2.0)

  ex = fill_kvector_components(kvector(blade(1, 2), blade(2, 3)), sig)
  @test ex == kvector(blade(1, 2), weighted(blade(1, 3), 0), blade(2, 3))
end;
