using SymbolicGA: infer_grade, project!, dereference

equals(x::Expression, y::Expression) = x.head == y.head && all(equals(dereference(x.cache, xx), dereference(y.cache, yy)) for (xx, yy) in zip(x, y)) && x.grade === y.grade
equals(x, y) = x == y

# Do not cache expressions in-between tests.
new_expression(sig::Signature, head, args...) = Expression(ExpressionCache(sig), head, args...)

SymbolicGA.is_expression_caching_enabled() = false

sig = Signature(3, 1)
cache = ExpressionCache(sig)

@testset "Expressions" begin
  @testset "Expression basics" begin
    @test isexpr(factor(cache, 0), FACTOR, 1)
    @test !isexpr(blade(cache, 1, 2), BLADE, 1)
    @test isexpr(blade(cache, 1, 2), BLADE, 2)
    @test isexpr(blade(cache, 1, 2), (FACTOR, BLADE))

    ex = blade(cache, 1, 2)
    ex2 = postwalk(x -> isa(dereference(cache, x), Int) ? dereference(cache, x) + 1 : x, ex)
    @test ex2 == blade(cache, 2, 3)
  end

  @testset "Grade inference" begin
    @test infer_grade(cache, FACTOR, 0) == 0
    @test infer_grade(cache, BLADE, [1, 2, 3]) == 3
    @test infer_grade(cache, BLADE, [1, 2, 3, 3]) == 2
    @test infer_grade(cache, BLADE, [1, 2, 3, 3, 3]) == 3
    @test infer_grade(cache, BLADE, [1, 1, 2, 2, 3, 3, 3]) == 1
    @test infer_grade(cache, BLADE, [1, 2, 3, 1, 2, 4, 1, 2]) == 4
    @test infer_grade(cache, KVECTOR, [blade(cache, 1, 2), blade(cache, 2, 3)]) == 2
    @test infer_grade(cache, MULTIVECTOR, [blade(cache, 1, 2), blade(cache, 2, 3)]) == 2
    @test infer_grade(cache, MULTIVECTOR, [kvector(blade(cache, 1, 2), blade(cache, 2, 3)), kvector(blade(cache, 1))]) == [1, 2]

    cache_3 = ExpressionCache(Signature(3))
    ex = blade(cache_3, 1, 2)
    @test grade(ex) == 2
    @test antigrade(ex) == 1
    ex = blade(cache_3, 1, 2) + blade(cache_3, 1)
    @test grade(ex) == [1, 2]
    @test antigrade(ex) == [2, 1]
  end

  @testset "Blades and metric simplifications" begin
    ex = blade(cache, 1, 1)
    @test ex.grade == 0
    @test ex == scalar(cache, 1)

    ex = blade(cache, 1, 2, 3, 1)
    @test ex.grade == 2
    @test ex == blade(cache, 2, 3)

    ex = blade(cache, 4, 4)
    @test ex.grade == 0
    @test ex == scalar(cache, -1)

    ex = blade(cache, 1, 2, 1)
    @test ex.grade == 1
    @test ex == weighted(blade(cache, 2), -1)

    @test blade(cache, 1) * blade(cache, 2) == blade(cache, 1, 2)
    @test blade(cache, 1, 2) * blade(cache, 3) == blade(cache, 1, 2, 3)
  end

  @testset "Simplification of null elements in addition" begin
    @test factor(cache, 1) + factor(cache, 0) == factor(cache, 1)
    @test blade(cache, 1) + factor(cache, 0) == blade(cache, 1)
  end

  @testset "Disassociation of products and sums" begin
    @test factor(cache, 1) * (factor(cache, :x) * factor(cache, :y)) == factor(cache, :(x * y))
    @test factor(cache, :z) * (factor(cache, :x) * factor(cache, :y)) == factor(cache, :(z * x * y))
    @test factor(cache, 1) + (factor(cache, :x) + factor(cache, :y)) == factor(cache, :(x + y + 1))
    @test factor(cache, 1) + (factor(cache, 2) + factor(cache, 0)) == factor(cache, 3)
  end

  @testset "Distribution of products" begin
    @test (factor(cache, :x) + factor(cache, :y)) * (factor(cache, :w) + factor(cache, :z)) == factor(cache, :((x + y) * (w + z)))
    @test (blade(cache, 1) + blade(cache, 3)) * (blade(cache, 2) + blade(cache, 4)) == Expression(ADDITION, blade(cache, 1, 2), blade(cache, 1, 4), blade(cache, 3, 2), blade(cache, 3, 4), cache; simplify = false)
    @test (factor(cache, :x) + blade(cache, 1)) * (factor(cache, :y) + blade(cache, 4)) == Expression(ADDITION, factor(cache, :(x * y)), factor(cache, :x) * blade(cache, 4), blade(cache, 1) * factor(cache, :y), blade(cache, 1, 4), cache; simplify = false)
    @test (factor(cache, :x) + factor(cache, :y)) * blade(cache, 1, 2) == Expression(GEOMETRIC_PRODUCT, factor(cache, :x) + factor(cache, :y), blade(cache, 1, 2), cache; simplify = false)
  end

  @testset "Simplification and canonicalization of factors" begin
    @test blade(cache, 1, 2) * factor(cache, 3) == Expression(GEOMETRIC_PRODUCT, [factor(cache, 3), blade(cache, 1, 2)], cache; simplify = false)
    @test blade(cache, 1, 2) * factor(cache, 3) * factor(cache, 5) == Expression(GEOMETRIC_PRODUCT, factor(cache, 15), blade(cache, 1, 2), cache; simplify = false)
    @test blade(cache, 1, 2) * factor(cache, :x) * factor(cache, :(y[1])) == Expression(GEOMETRIC_PRODUCT, factor(cache, :(x * y[1])), blade(cache, 1, 2), cache; simplify = false)
    @test factor(cache, 1) * blade(cache, 1, 2) * factor(cache, 3) == Expression(GEOMETRIC_PRODUCT, factor(cache, 3), blade(cache, 1, 2); simplify = false)
    # @test blade(cache, 1, 2) * factor(cache, 0) == Expression(GEOMETRIC_PRODUCT, factor(cache, 0), blade(cache, 1, 2); simplify = false)
    @test blade(cache, 1, 2) * factor(cache, 0) == factor(cache, 0)
    @test factor(cache, :(-1 * -1)) == factor(cache, 1)
    @test factor(cache, :(-1 * -1)) * blade(cache, 1, 2) == blade(cache, 1, 2)

    @testset "Simplification of additive factors" begin
      @test factor(cache, :x) + factor(cache, :y) == factor(cache, :(x + y))
      @test factor(cache, :x) + factor(cache, 1) == factor(cache, :(x + 1))
      @test factor(cache, 2) + factor(cache, 3) == factor(cache, 5)
      @test factor(cache, :x) + factor(cache, 3) + factor(cache, :y) + factor(cache, 2) == factor(cache, :(x + y + 5))

      @test scalar(cache, :x) - scalar(cache, :x) == factor(cache, 0)
      @test scalar(cache, :x) + scalar(cache, :x) == scalar(cache, :(2x))
      @test scalar(cache, :x) + scalar(cache, :y) == scalar(cache, :(x + y))
      @test scalar(cache, :x) + scalar(cache, :x) - scalar(cache, :(2x)) == factor(cache, 0)
      @test scalar(cache, :(x * y)) - scalar(cache, :(x * y)) == factor(cache, 0)
      @test scalar(cache, :(x * y)) + scalar(cache, :(x * y)) ≠ factor(cache, 0)
      @test scalar(cache, :x) - scalar(cache, :(2x)) == scalar(cache, :(-1 * x))
    end
  end

  @testset "Blade grouping over addition" begin
    x = weighted(blade(cache, 1, 3), :x)
    ex = x + x
    @test ex == weighted(blade(cache, 1, 3), :(2x))

    ex = scalar(cache, :x) + scalar(cache, :y)
    @test ex == scalar(cache, :(x + y))

    ex = weighted(blade(cache, 1), :x) + weighted(blade(cache, 2, 3), :y) + weighted(blade(cache, 1), :z)
    @test ex == weighted(blade(cache, 2, 3), :y) + weighted(blade(cache, 1), :(x + z))
  end

  @testset "Projections" begin
    @test project!(factor(cache, :x), 1) == factor(cache, 0)
    @test project!(factor(cache, :x), 0) == factor(cache, :x)
    @test project!(blade(cache, 1, 2), 1) == factor(cache, 0)
    @test project!(blade(cache, 1, 2) + blade(cache, 1), 1) == blade(cache, 1)
    @test project!(blade(cache, 1) + blade(cache, 1, 2) + blade(cache, 1, 2, 3), 2) == blade(cache, 1, 2)
  end

  @testset "Reversions" begin
    @test reverse(blade(cache, 1, 2)) == blade(cache, 2, 1)
    @test reverse(factor(cache, :x) * blade(cache, 1, 2)) == factor(cache, :x) * blade(cache, 2, 1)
    @test reverse(blade(cache, 1, 2, 3) + blade(cache, 2) + blade(cache, 2, 3)) == blade(cache, 3, 2, 1) + blade(cache, 2) + blade(cache, 3, 2)

    @test antireverse(sig, blade(cache, 1, 2)) == blade(cache, 2, 1)
    @test antireverse(sig, factor(cache, :x) * blade(cache, 1, 2)) == factor(cache, :x) * blade(cache, 2, 1)
    @test antireverse(sig, blade(cache, 1, 2, 3) + blade(cache, 2) + blade(cache, 2, 3)) == blade(cache, 1, 2, 3) - blade(cache, 2) + blade(cache, 3, 2)
    @test antireverse(Signature(3, 0, 1), weighted(blade(cache, 4), 1.0)) == antireverse(Signature(3, 0, 1), blade(cache, 4)) == -blade(cache, 4)
  end

  @testset "Exterior products" begin
    x = blade(cache, 1, 2)
    y = blade(cache, 3)

    @test exterior_product(blade(cache, 1, 2), blade(cache, 3)) == blade(cache, 1, 2, 3)
    @test exterior_product(blade(cache, 1, 2), blade(cache, 2)) == factor(cache, 0)
  end

  @testset "Common operators" begin
    @test Expression(cache, INTERIOR_PRODUCT, blade(cache, 1), blade(cache, 1, 2)) == blade(cache, 2)
    @test Expression(cache, COMMUTATOR_PRODUCT, blade(cache, 1), blade(cache, 2)) == weighted(blade(cache, 1, 2), 0.5) + weighted(blade(cache, 1, 2), 0.5)
    @test Expression(cache, EXTERIOR_PRODUCT, blade(cache, 1), blade(cache, 2)) == blade(cache, 1, 2)
  end

  @testset "Inversion" begin
    @test simplified(INVERSE, factor(cache, 2.0)) == factor(cache, 0.5)
    @test simplified(INVERSE, factor(cache, :x)) == factor(cache, :($inv(x)))
    @test Expression(cache, INVERSE, scalar(cache, 2.0)) == scalar(cache, 0.5)
    @test Expression(cache, INVERSE, blade(cache, 1, 2)) == weighted(blade(cache, 1, 2), -1)
    @test Expression(cache, INVERSE, weighted(blade(cache, 1, 2), 5.0)) == weighted(blade(cache, 1, 2), -0.2)
    versor = simplified(ADDITION, scalar(cache, 2.0), weighted(blade(cache, 1, 2), 5.0))
    @test Expression(cache, GEOMETRIC_PRODUCT, versor, Expression(cache, INVERSE, versor)) == scalar(cache, 1.0)
    mv = simplified(ADDITION, scalar(cache, 2.0), weighted(blade(cache, 3), 1.2), weighted(blade(cache, 1, 2), 5.0))
    @test_throws "only supported for versors" Expression(cache, GEOMETRIC_PRODUCT, mv, Expression(cache, INVERSE, mv)) == scalar(cache, 1.0)
  end

  @testset "Exponentiation" begin
    sig = Signature(3, 0, 1)
    b = blade(cache, 1, 2, 4)
    ex = Expression(cache, EXPONENTIAL, b)
    @test ex == simplified(ADDITION, scalar(cache, 1), b)

    sig = Signature(3)
    for α in (1, 3.2)
      b = weighted(blade(cache, 1, 2), α)
      ex = Expression(cache, EXPONENTIAL, b)
      @test grade(ex) == [0, 2]
      @test ex == simplified(ADDITION, scalar(cache, cos(α)), simplified(GEOMETRIC_PRODUCT, scalar(cache, sin(α) / α), b))
    end
  end
end;
