using LazyGeometricAlgebra: restructure_sums, fill_kvector_components, group_kvector_blades, Zero

@testset "Passes" begin
  x, y = factor(:x) * blade(1, 3), factor(:x) * blade(1, 2)
  ex = restructure_sums(x + y)
  @test ex == kvector(x, y)

  z = factor(:x) * blade(1, 2, 3)
  ex = restructure_sums(x + y + z)
  @test ex == multivector(kvector(x, y), z)
  @test isa(repr(ex), String)

  ex = kvector(x, x)
  ex2 = group_kvector_blades(ex)
  @test ex2 == kvector(weighted(blade(1, 3), :(x + x)))

  ex = kvector(scalar(:x), scalar(:y))
  ex2 = group_kvector_blades(ex)
  @test ex2 == kvector(scalar(:(x + y)))

  ex = fill_kvector_components(kvector(blade(1, 2), blade(2, 3)), Signature(3))
  @test ex == kvector(blade(1, 2), weighted(blade(1, 3), Zero()), blade(2, 3))
end;
