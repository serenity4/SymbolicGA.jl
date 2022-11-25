using LazyGeometricAlgebra: restructure_sums, disassociate_kvectors, fill_kvector_components

@testset "Passes" begin
  x, y = scalar(:x) * blade(1, 3), scalar(:x) * blade(1, 2)
  ex = restructure_sums(x + y)
  @test ex == kvector(x, y)

  z = scalar(:x) * blade(1, 2, 3)
  ex = restructure_sums(x + y + z)
  @test ex == multivector(kvector(x, y), z)
  @test isa(repr(ex), String)

  ex = fill_kvector_components(kvector(blade(1, 2), blade(2, 3)), Signature(3))
  @test ex == kvector(blade(1, 2), scalar(0), blade(2, 3))
end;
