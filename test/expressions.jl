using LazyGeometricAlgebra, Test
using LazyGeometricAlgebra: Expression, isexpr, isgrade

@testset "Expressions" begin
  ex = Expression(:scalar, 0)
  @test ex.grade == 0

  ex = Expression(:basis, 1)
  @test ex.grade == 1

  ex = Expression(:blade, Expression(:basis, 1), Expression(:basis, 2))
  @test ex.grade == 2

  ex = Expression(:blade, Expression(:basis, 1), Expression(:basis, 1))
  @test ex.grade == 0

  ex = Expression(:blade, Expression(:basis, 1), Expression(:basis, 2), Expression(:basis, 3), Expression(:basis, 1))
  @test ex.grade == 2
end;
