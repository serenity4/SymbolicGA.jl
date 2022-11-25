using LazyGeometricAlgebra: distribute, restructure_sums, canonicalize_blades, apply_metric, disassociate_kvectors, fill_kvector_components, apply_reverse_operators

@testset "Passes" begin
  x, y = weighted(blade(1, 3), 2.0), weighted(blade(1, 2), 1.2)
  ex = restructure_sums(x + y)
  @test ex == kvector(x, y)

  z = weighted(blade(1, 2, 3), 1.2)
  ex = restructure_sums(x + y + z)
  @test ex == multivector(kvector(x, y), z)
  @test isa(repr(ex), String)

  ex = fill_kvector_components(kvector(blade(1, 2), blade(2, 3)), sig)
  @test ex == kvector(blade(1, 2), weighted(blade(1, 3), 0), blade(2, 3))
end;
