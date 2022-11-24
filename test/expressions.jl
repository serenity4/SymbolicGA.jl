using LazyGeometricAlgebra: infer_grade

sig = Signature(3, 1)

@testset "Expressions" begin
  @testset "Expression basics" begin
    @test isexpr(scalar(0), :scalar, 1)
    @test !isexpr(blade(1, 2), :blade, 1)
    @test isexpr(blade(1, 2), :blade, 2)
    @test isexpr(blade(1, 2), (:scalar, :blade))

    ex = blade(1, 2)
    ex2 = postwalk(x -> isa(x, Int) ? x + 1 : x, ex)
    @test ex2 == blade(2, 3)
  end

  @testset "Grade inference" begin
    @test infer_grade(:scalar, 0) == 0
    @test infer_grade(:blade, [1, 2, 3]) == 3
    @test infer_grade(:blade, [1, 2, 3, 3]) == 2
    @test infer_grade(:blade, [1, 2, 3, 3, 3]) == 3
    @test infer_grade(:blade, [1, 1, 2, 2, 3, 3, 3]) == 1
    @test infer_grade(:blade, [1, 2, 3, 1, 2, 4, 1, 2]) == 4
    @test infer_grade(:kvector, [blade(1, 2), blade(2, 3)]) == 2
    @test infer_grade(:multivector, [blade(1, 2), blade(2, 3)]) == [2]
    @test infer_grade(:multivector, [kvector(blade(1, 2), blade(2, 3)), kvector(blade(1))]) == [1, 2]
  end

  @testset "Blades and metric simplifications" begin
    ex = blade(sig, 1, 1)
    @test ex.grade == 0
    @test ex == scalar(1)

    ex = blade(sig, 1, 2, 3, 1)
    @test ex.grade == 2
    @test ex == blade(2, 3)

    ex = blade(sig, 4, 4)
    @test ex.grade == 0
    @test ex == scalar(-1)

    ex = blade(sig, 1, 2, 1)
    @test ex.grade == 1
    @test ex == scalar(-1) * blade(2)

    @test blade(1) * blade(2) == blade(1, 2)
    @test blade(1, 2) * blade(3) == blade(1, 2, 3)
  end

  @testset "Simplification and canonicalization of scalar factors" begin
    @test blade(1, 2) * scalar(3) == Expression(:*, scalar(3), blade(1, 2); simplify = false)
    @test blade(1, 2) * scalar(3) * scalar(5) == Expression(:*, scalar(:(3 * 5)), blade(1, 2); simplify = false)
    @test blade(1, 2) * scalar(:x) * scalar(:(y[1])) == Expression(:*, scalar(:(x * y[1])), blade(1, 2); simplify = false)
    @test scalar(1) * blade(1, 2) * scalar(3) == Expression(:*, scalar(3), blade(1, 2); simplify = false)
    @test blade(1, 2) * scalar(0) == scalar(0)
    @test scalar(inv(scalar(5))) * scalar(5) == scalar(0.2) * scalar(5) == scalar(:(0.2 * 5))
  end

  @testset "Simplification of null scalars in addition" begin
    @test scalar(1) + scalar(0) == Expression(:scalar, 1; simplify = false)
    @test blade(1) + scalar(0) == Expression(:blade, 1; simplify = false)
  end

  @testset "Disassociation of products and sums" begin
    @test scalar(1) * (scalar(2) * scalar(3)) == scalar(:(2 * 3))
    @test scalar(4) * (scalar(2) * scalar(3)) == scalar(:(4 * (2 * 3)))
    @test scalar(1) + (scalar(2) + scalar(3)) == scalar(:(1 + (2 + 3)))
    @test scalar(1) + (scalar(2) + scalar(0)) == scalar(:(1 + 2))
  end

  @testset "Distribution of products" begin
    # Scalars operations are not distributed.
    @test (scalar(:x) + scalar(:y)) * (scalar(:w) + scalar(:z)) == scalar(:((x + y) * (w + z)))

    @test (blade(1) + blade(3)) * (blade(2) + blade(4)) == Expression(:+, blade(1, 2), blade(1, 4), blade(3, 2), blade(3, 4); simplify = false)
    @test (scalar(:x) + blade(1)) * (scalar(:y) + blade(4)) == Expression(:+, scalar(:(x * y)), scalar(:x) * blade(4), blade(1) * scalar(:y), blade(1, 4); simplify = false)
  end
end;
