using LazyGeometricAlgebra: infer_grade, project!

sig = Signature(3, 1)

@testset "Expressions" begin
  @testset "Expression basics" begin
    @test isexpr(factor(0), :factor, 1)
    @test !isexpr(blade(1, 2), :blade, 1)
    @test isexpr(blade(1, 2), :blade, 2)
    @test isexpr(blade(1, 2), (:factor, :blade))

    ex = blade(1, 2)
    ex2 = postwalk(x -> isa(x, Int) ? x + 1 : x, ex)
    @test ex2 == blade(2, 3)
  end

  @testset "Grade inference" begin
    @test infer_grade(:factor, 0) == 0
    @test infer_grade(:blade, [1, 2, 3]) == 3
    @test infer_grade(:blade, [1, 2, 3, 3]) == 2
    @test infer_grade(:blade, [1, 2, 3, 3, 3]) == 3
    @test infer_grade(:blade, [1, 1, 2, 2, 3, 3, 3]) == 1
    @test infer_grade(:blade, [1, 2, 3, 1, 2, 4, 1, 2]) == 4
    @test infer_grade(:kvector, [blade(1, 2), blade(2, 3)]) == 2
    @test infer_grade(:multivector, [blade(1, 2), blade(2, 3)]) == 2
    @test infer_grade(:multivector, [kvector(blade(1, 2), blade(2, 3)), kvector(blade(1))]) == [1, 2]

    ex = blade(1, 2)
    @test grade(ex) == 2
    @test antigrade(Signature(3), ex) == 1
    ex = blade(1, 2) + blade(1)
    @test grade(ex) == [1, 2]
    @test antigrade(Signature(3), ex) == [2, 1]
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
    @test ex == weighted(blade(2), -1)

    @test blade(1) * blade(2) == blade(1, 2)
    @test blade(1, 2) * blade(3) == blade(1, 2, 3)
  end

  @testset "Simplification and canonicalization of factors" begin
    @test blade(1, 2) * factor(3) == Expression(:⟑, factor(3), blade(1, 2); simplify = false)
    @test blade(1, 2) * factor(3) * factor(5) == Expression(:⟑, factor(15), blade(1, 2); simplify = false)
    @test blade(1, 2) * factor(:x) * factor(:(y[1])) == Expression(:⟑, factor(:(x * y[1])), blade(1, 2); simplify = false)
    @test factor(1) * blade(1, 2) * factor(3) == Expression(:⟑, factor(3), blade(1, 2); simplify = false)
    # @test blade(1, 2) * factor(0) == Expression(:⟑, factor(0), blade(1, 2); simplify = false)
    @test blade(1, 2) * factor(0) == factor(0)
    @test inv(scalar(5)) * scalar(5) == scalar(0.2) * scalar(5) == scalar(1.0)
    @test factor(:(-1 * -1)) == factor(1)
    @test factor(:(-1 * -1)) * blade(1, 2) == blade(1, 2)

    @testset "Simplification of additive factors" begin
      @test factor(:x) + factor(:y) == factor(:(x + y))
      @test factor(:x) + factor(1) == factor(:(x + 1))
      @test factor(2) + factor(3) == factor(5)
      @test factor(:x) + factor(3) + factor(:y) + factor(2) == factor(:(x + y + 5))
    end
  end

  @testset "Simplification of null elements in addition" begin
    @test factor(1) + factor(0) == Expression(:factor, 1; simplify = false)
    @test blade(1) + factor(0) == Expression(:blade, 1; simplify = false)
  end

  @testset "Disassociation of products and sums" begin
    @test factor(1) * (factor(:x) * factor(:y)) == factor(:(x * y))
    @test factor(:z) * (factor(:x) * factor(:y)) == factor(:(z * (x * y)))
    @test factor(1) + (factor(:x) + factor(:y)) == factor(:((x + y) + 1))
    @test factor(1) + (factor(2) + factor(0)) == factor(3)
  end

  @testset "Distribution of products" begin
    @test (factor(:x) + factor(:y)) * (factor(:w) + factor(:z)) == factor(:((x + y) * (w + z)))
    @test (blade(1) + blade(3)) * (blade(2) + blade(4)) == Expression(:+, blade(1, 2), blade(1, 4), blade(3, 2), blade(3, 4); simplify = false)
    @test (factor(:x) + blade(1)) * (factor(:y) + blade(4)) == Expression(:+, factor(:(x * y)), factor(:x) * blade(4), blade(1) * factor(:y), blade(1, 4); simplify = false)
    @test (factor(:x) + factor(:y)) * blade(1, 2) == Expression(:*, factor(:x) + factor(:y), blade(1, 2); simplify = false)
  end

  @testset "Projections" begin
    @test project!(factor(:x), 1) == factor(0)
    @test project!(factor(:x), 0) == factor(:x)
    @test project!(blade(1, 2), 1) == factor(0)
    @test project!(blade(1, 2) + blade(1), 1) == blade(1)
    @test project!(blade(1) + blade(1, 2) + blade(1, 2, 3), 2) == blade(1, 2)
  end

  @testset "Reversions" begin
    @test reverse(blade(1, 2)) == blade(2, 1)
    @test reverse(factor(:x) * blade(1, 2)) == factor(:x) * blade(2, 1)
    @test reverse(blade(1, 2, 3) + blade(2) + blade(2, 3)) == blade(3, 2, 1) + blade(2) + blade(3, 2)

    @test antireverse(sig, blade(1, 2)) == blade(2, 1)
    @test antireverse(sig, factor(:x) * blade(1, 2)) == factor(:x) * blade(2, 1)
    @test antireverse(sig, blade(1, 2, 3) + blade(2) + blade(2, 3)) == blade(1, 2, 3) - blade(2) + blade(3, 2)
  end

  @testset "Exterior products" begin
    x = blade(1, 2)
    y = blade(3)

    @test exterior_product(blade(1, 2), blade(3)) == blade(1, 2, 3)
    @test exterior_product(sig, blade(1, 2), blade(2)) == factor(0)
  end

  @testset "Common operators" begin
    @test simplified(sig, :⦿, blade(1, 2), blade(1, 2)) == scalar(-1)
    @test simplified(sig, :●, blade(1), blade(1, 2)) == simplified(sig, :⋅, blade(1), blade(1, 2)) == blade(2)
    @test simplified(sig, :×, blade(1), blade(2)) == weighted(blade(1, 2), 0.5) + weighted(blade(1, 2), 0.5)
    @test simplified(sig, :inverse, blade(1)) == blade(1)
    @test simplified(sig, :inverse, factor(0.5) * blade(1)) == weighted(blade(1), 2.0)
    @test simplified(sig, :∧, blade(1), blade(2)) == blade(1, 2)
  end
end;
