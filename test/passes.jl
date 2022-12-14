using LazyGeometricAlgebra: restructure_sums, fill_kvector_components, Zero

@testset "Passes" begin
  sig = Signature(3)
  x, y = factor(:x) * blade(1, 3), factor(:x) * blade(1, 2)
  z = factor(:x) * blade(1, 2, 3)

  ex = restructure_sums(x + y, sig)
  @test ex == kvector(x, y)

  ex = restructure_sums(x + y + z, sig)
  @test ex == multivector(kvector(x, y), kvector(z))
  @test isa(repr(ex), String)

  ex = restructure_sums(z, sig)
  @test ex == kvector(z)

  ex = fill_kvector_components(kvector(blade(1, 2), blade(2, 3)), Signature(3))
  @test ex == kvector(blade(1, 2), weighted(blade(1, 3), Zero()), blade(2, 3))
end;
