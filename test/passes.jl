using SymbolicGA: restructure_sums, fill_kvector_components, Zero

@testset "Passes" begin
  cache = ExpressionCache(Signature(3))
  x, y = factor(cache, :x) * blade(cache, 1, 3), factor(cache, :x) * blade(cache, 1, 2)
  z = factor(cache, :x) * blade(cache, 1, 2, 3)

  ex = restructure_sums(x + y)
  @test ex == kvector(x, y)

  ex = restructure_sums(x + y + z)
  @test ex == multivector(kvector(x, y), kvector(z))
  @test isa(repr(ex), String)

  ex = restructure_sums(z)
  @test ex == kvector(z)

  ex = fill_kvector_components(kvector(blade(cache, 1, 2), blade(cache, 2, 3)))
  @test ex == kvector(blade(cache, 1, 2), weighted(blade(cache, 1, 3), Zero()), blade(cache, 2, 3))
end;
