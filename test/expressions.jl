using LazyGeometricAlgebra: extract_grade

@testset "Expressions" begin
  @test extract_grade(:scalar, 0) == 0
  @test extract_grade(:basis, basis.([1])) == 1
  @test extract_grade(:basis, basis.([3])) == 1
  @test extract_grade(:blade, basis.([1, 2, 3])) == 3
  @test extract_grade(:blade, basis.([1, 2, 3, 3])) == 2
  @test extract_grade(:blade, basis.([1, 2, 3, 3, 3])) == 3
  @test extract_grade(:blade, basis.([1, 1, 2, 2, 3, 3, 3])) == 1
  @test extract_grade(:blade, basis.([1, 2, 3, 1, 2, 4, 1, 2])) == 4
  @test extract_grade(:kvector, [blade(1, 2), blade(2, 3)]) == 2
  @test extract_grade(:multivector, [blade(1, 2), blade(2, 3)]) == 2
  @test extract_grade(:multivector, [kvector(blade(1, 2), blade(2, 3)), kvector(blade(1))]) === nothing

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

  ex2 = postwalk(x -> isexpr(x, :basis) ? basis(basis_index(x) + 1) : x, ex)
  @test ex2 == blade(2, 3, 4, 2)

  @test blade(1, 2) * basis(3) == blade(1, 2, 3)

  @test -basis(1) == weighted(basis(1), -1)
  @test basis(1) * scalar(1) == basis(1)
  @test scalar(1) * scalar(2) == scalar(2)
  @test basis(1) - basis(2) == basis(1) + -basis(2)
end;
