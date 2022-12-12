using LazyGeometricAlgebra: infer_grade, project!

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
    @test ex == scalar(-1) * blade(2)

    @test blade(1) * blade(2) == blade(1, 2)
    @test blade(1, 2) * blade(3) == blade(1, 2, 3)
  end

  @testset "Simplification and canonicalization of scalar factors" begin
    @test blade(1, 2) * scalar(3) == Expression(:⟑, scalar(3), blade(1, 2); simplify = false)
    @test blade(1, 2) * scalar(3) * scalar(5) == Expression(:⟑, scalar(:(3 * 5)), blade(1, 2); simplify = false)
    @test blade(1, 2) * scalar(:x) * scalar(:(y[1])) == Expression(:⟑, scalar(:(x * y[1])), blade(1, 2); simplify = false)
    @test scalar(1) * blade(1, 2) * scalar(3) == Expression(:⟑, scalar(3), blade(1, 2); simplify = false)
    @test blade(1, 2) * scalar(0) == scalar(0)
    @test scalar(inv(scalar(5))) * scalar(5) == scalar(0.2) * scalar(5) == scalar(:(0.2 * 5))
    @test scalar(:(-1 * -1)) == scalar(1)
    @test scalar(:(-1 * -1)) * blade(1, 2) == blade(1, 2)
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

  @testset "Projections" begin
    @test project!(scalar(:x), 1) == scalar(0)
    @test project!(scalar(:x), 0) == scalar(:x)
    @test project!(blade(1, 2), 1) == scalar(0)
    @test project!(blade(1, 2) + blade(1), 1) == blade(1)
    @test project!(blade(1) + blade(1, 2) + blade(1, 2, 3), 2) == blade(1, 2)
  end

  @testset "Reversions" begin
    @test reverse(blade(1, 2)) == blade(2, 1)
    @test reverse(scalar(:x) * blade(1, 2)) == scalar(:x) * blade(2, 1)
    @test reverse(blade(1, 2, 3) + blade(2) + blade(2, 3)) == blade(3, 2, 1) + blade(2) + blade(3, 2)

    @test antireverse(sig, blade(1, 2)) == blade(2, 1)
    @test antireverse(sig, scalar(:x) * blade(1, 2)) == scalar(:x) * blade(2, 1)
    @test antireverse(sig, blade(1, 2, 3) + blade(2) + blade(2, 3)) == blade(1, 2, 3) - blade(2) + blade(3, 2)
  end

  @testset "Exterior products" begin
    x = blade(1, 2)
    y = blade(3)

    @test exterior_product(blade(1, 2), blade(3)) == blade(1, 2, 3)
    @test exterior_product(sig, blade(1, 2), blade(2)) == scalar(0)

    # @testset "De Morgan laws" begin
    #   @test right_complement(sig, exterior_product(x, y)) == exterior_antiproduct(sig, right_complement(sig, a), right_complement(sig, b))
    #   @test right_complement(sig, exterior_antiproduct(sig, x, y)) == exterior_product(right_complement(sig, a), right_complement(sig, b))
    #   @test left_complement(exterior_product(x, y)) == exterior_antiproduct(sig, left_complement(a), left_complement(b))
    #   @test left_complement(exterior_antiproduct(sig, x, y)) == exterior_product(left_complement(a), left_complement(b))
    # end
  end

  @testset "Complements" begin
    sig = Signature(3, 0, 1)

    b = blade(1, 3)
    b̅ = right_complement(sig, b)
    @test b̅ == blade(4, 2)

    b̅̅ = right_complement(sig, b̅)
    @test b̅̅ == b
    @test right_complement(sig, right_complement(sig, b)) == b

    b = blade(4)
    b̅ = right_complement(sig, b)
    @test b̅ == blade(3, 2, 1)

    b̅̅ = right_complement(sig, b̅)
    @test b̅̅ == -b
    @test simplified(sig, :∧, b, b̅) == pseudoscalar(sig)
  end

  @testset "Common operators" begin
    @test simplified(sig, :⦿, blade(1, 2), blade(1, 2)) == scalar(-1)
    @test simplified(sig, :●, blade(1), blade(1, 2)) == simplified(sig, :⋅, blade(1), blade(1, 2)) == blade(2)
    @test simplified(sig, :×, blade(1), blade(2)) == scalar(0.5) * blade(1, 2) + scalar(0.5) * blade(1, 2)
    @test simplified(sig, :dual, blade(1)) == blade(4, 3, 2)
    @test simplified(sig, :inverse, blade(1)) == scalar(1.0) * blade(1)
    @test simplified(sig, :inverse, scalar(0.5) * blade(1)) == scalar(:(0.5 * inv(0.5 * 0.5))) * blade(1)
    @test simplified(sig, :∧, blade(1), blade(2)) == blade(1, 2)
  end
end;
