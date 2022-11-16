@testset "Expressions" begin
  ex = scalar(0)
  @test ex.grade == 0
  @test isexpr(ex, :scalar)

  ex = basis(1)
  @test ex.grade == 1
  @test isexpr(ex, :basis, 1)

  ex = blade(1, 2)
  @test !isexpr(ex, :blade, 1)
  @test isexpr(ex, :blade, 2)
  @test isexpr(ex, (:basis, :blade))
  @test ex.grade == 2

  ex = blade(1, 1)
  @test ex.grade == 0

  ex = blade(1, 2, 3, 1)
  @test ex.grade == 2

  ex2 = postwalk(x -> isexpr(x, :basis) ? basis(x[1] + 1) : x, ex)
  @test ex2 == blade(2, 3, 4, 2)

  @test blade(1, 2) * basis(3) == blade(1, 2, 3)
end;
