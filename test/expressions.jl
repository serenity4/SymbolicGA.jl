@testset "Expressions" begin
  ex = Expression(:scalar, 0)
  @test ex.grade == 0
  @test isexpr(ex, :scalar)

  ex = Expression(:basis, 1)
  @test ex.grade == 1
  @test isexpr(ex, :basis, 1)

  ex = Expression(:blade, Expression(:basis, 1), Expression(:basis, 2))
  @test !isexpr(ex, :blade, 1)
  @test isexpr(ex, :blade, 2)
  @test isexpr(ex, (:basis, :blade))
  @test ex.grade == 2

  ex = Expression(:blade, Expression(:basis, 1), Expression(:basis, 1))
  @test ex.grade == 0

  ex = Expression(:blade, Expression(:basis, 1), Expression(:basis, 2), Expression(:basis, 3), Expression(:basis, 1))
  @test ex.grade == 2

  ex2 = postwalk(x -> isexpr(x, :basis) ? Expression(:basis, x[1] + 1) : x, ex)
  @test ex2 == Expression(:blade, Expression(:basis, 2), Expression(:basis, 3), Expression(:basis, 4), Expression(:basis, 2))

  @test blade(1, 2) * basis(3) == blade(1, 2, 3)
end;
