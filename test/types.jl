@testset "User-facing types" begin
  k = KVector{1,3}((1, 2, 3))
  @test eltype(k) === Int
  @test collect(k) == [1, 2, 3] && isa(collect(k), Vector{Int})
  @test length(k) == 3
  @test_throws "Expected" KVector{1, 3}((1, 2, 3, 4))
  @test_throws "of dimension" KVector{4, 3}((1, 2, 3, 4))
  @test collect(KVector{0, 3}((1,))) == [1]
end;
