using SymbolicGA: infer_grade, project!, dereference, factorize_addition_terms, propagate_constants

sig = Signature(3, 1)
cache = ExpressionCache(sig)
sc(x) = scalar(cache, x)
x = sc(:x)
y = sc(:y)
e1, e2, e3 = blade.(cache, [1, 2, 3])

@testset "Expressions" begin
  @testset "Expression basics" begin
    @test isexpr(scalar(cache, 0), SCALAR, 1)
    @test !isexpr(blade(cache, 1, 2), BLADE, 1)
    @test isexpr(blade(cache, 1, 2), BLADE, 2)
    @test isexpr(blade(cache, 1, 2), (SCALAR, BLADE))

    ex = blade(cache, 1, 2)
    ex2 = postwalk(x -> isa(dereference(cache, x), Int) ? dereference(cache, x) + 1 : x, ex)
    @test ex2 == blade(cache, 2, 3)
  end

  @testset "Grade inference" begin
    @test infer_grade(cache, SCALAR, 0) == 0
    @test infer_grade(cache, BLADE, [1, 2, 3]) == 3
    @test infer_grade(cache, BLADE, [1, 2, 3, 3]) == 2
    @test infer_grade(cache, BLADE, [1, 2, 3, 3, 3]) == 3
    @test infer_grade(cache, BLADE, [1, 1, 2, 2, 3, 3, 3]) == 1
    @test infer_grade(cache, BLADE, [1, 2, 3, 1, 2, 4, 1, 2]) == 4
    @test infer_grade(cache, KVECTOR, [blade(cache, 1, 2), blade(cache, 2, 3)]) == 2
    @test infer_grade(cache, MULTIVECTOR, [blade(cache, 1, 2), blade(cache, 2, 3)]) == 2
    @test infer_grade(cache, MULTIVECTOR, [kvector(blade(cache, 1, 2), blade(cache, 2, 3)), kvector(blade(cache, 1))]) == [1, 2]

    _cache = ExpressionCache(Signature(3))
    ex = blade(_cache, 1, 2)
    @test grade(ex) == 2
    @test antigrade(ex) == 1
    ex = blade(_cache, 1, 2) + blade(_cache, 1)
    @test grade(ex) == [1, 2]
    @test antigrade(ex) == [2, 1]
  end

  @testset "Factorization" begin
    terms = factorize_addition_terms(cache, [x, 3x])
    @test terms == [4x]

    terms = factorize_addition_terms(cache, [x, -1x])
    @test terms == []

    terms = factorize_addition_terms(cache, [y*x, -1y*x, x, 2y])
    @test terms == [x, 2y]

    terms = factorize_addition_terms(cache, [y*x, -1x*y, x, 2y])
    @test terms == [y*x, -1x*y, x, 2y]

    terms = factorize_addition_terms(cache, [x * e1, -1x * e1, e2, y * e3])
    @test terms == [e2, y * e3]
  end

  @testset "Constant propagation" begin
    terms = propagate_constants(cache, SCALAR_ADDITION, sc.([-1, 1, 2, :x]))
    @test terms == [sc(2), x]

    terms = propagate_constants(cache, SCALAR_PRODUCT, sc.([-1, 1, 2, :x]))
    @test terms == [sc(-2), x]

    terms = propagate_constants(cache, SCALAR_PRODUCT, sc.([-1, 0, 2, :x]))
    @test terms == []
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
    @test sc(1) + sc(0) == sc(1)
    @test blade(cache, 1) + sc(0) == blade(cache, 1)
  end

  @testset "Disassociation of products and sums" begin
    @test sc(1) * (sc(:x) * sc(:y)) == sc(:x) * sc(:y)
    @test sc(:z) * (sc(:x) * sc(:y)) == sc(:z) * sc(:x) * sc(:y)
    @test sc(1) + (sc(:x) + sc(:y)) == sc(1) + sc(:x) + sc(:y)
    @test sc(1) + (sc(2) + sc(0)) == sc(3)
  end

  @testset "Distribution of products" begin
    @test (blade(cache, 1) + blade(cache, 3)) * (blade(cache, 2) + blade(cache, 4)) == blade(cache, 1, 2) + blade(cache, 1, 4) + blade(cache, 3, 2) + blade(cache, 3, 4)
    @test (sc(:x) + blade(cache, 1)) * (sc(:y) + blade(cache, 4)) == sc(:x) * sc(:y) + sc(:x) * blade(cache, 4) + blade(cache, 1) * sc(:y) + blade(cache, 1, 4)
    @test (sc(:x) + sc(:y)) * blade(cache, 1, 2) == (sc(:x) + sc(:y)) ⟑ blade(cache, 1, 2)
  end

  @testset "Simplification and canonicalization of factors" begin
    @test blade(cache, 1, 2) * sc(3) == sc(3) ⟑ blade(cache, 1, 2)
    @test blade(cache, 1, 2) * sc(3) * sc(5) == sc(15) ⟑ blade(cache, 1, 2)
    @test blade(cache, 1, 2) * sc(:x) * scalar(cache, :(y[1])) == scalar(cache, :x) * scalar(cache, :(y[1])) ⟑ blade(cache, 1, 2)
    @test scalar(cache, 1) * blade(cache, 1, 2) * scalar(cache, 3) == scalar(cache, 3) ⟑ blade(cache, 1, 2)
    # @test blade(cache, 1, 2) * scalar(cache, 0) == Expression(GEOMETRIC_PRODUCT, scalar(cache, 0), blade(cache, 1, 2); simplify = false)
    @test blade(cache, 1, 2) * scalar(cache, 0) == scalar(cache, 0)
    @test scalar(cache, -1) * scalar(cache, -1) == scalar(cache, 1)
    @test scalar(cache, -1) * scalar(cache, -1) * blade(cache, 1, 2) == blade(cache, 1, 2)

    @testset "Simplification of additive factors" begin
      @test scalar(cache, 2) + scalar(cache, 3) == scalar(cache, 5)
      @test scalar(cache, :x) + scalar(cache, 3) + scalar(cache, :y) + scalar(cache, 2) == scalar(cache, 5) + scalar(cache, :x) + scalar(cache, :y)

      @test scalar(cache, :x) - scalar(cache, :x) == scalar(cache, 0)
      @test scalar(cache, :x) + scalar(cache, :x) == scalar(cache, 2) * scalar(cache, :x)
      @test scalar(cache, :x) + scalar(cache, :x) - scalar(cache, 2) * scalar(cache, :x) == scalar(cache, 0)
      @test scalar(cache, :x) - scalar(cache, 2) * scalar(cache, :x) == scalar(cache, -1) * scalar(cache, :x)
    end
  end

  @testset "Blade grouping over addition" begin
    x = weighted(blade(cache, 1, 3), :x)
    ex = x + x
    @test ex == weighted(blade(cache, 1, 3), scalar(cache, 2) * scalar(cache, :x))

    ex = weighted(blade(cache, 1), :x) + weighted(blade(cache, 2, 3), :y) + weighted(blade(cache, 1), :z)
    @test ex == weighted(blade(cache, 2, 3), :y) + blade(cache, 1) * scalar(cache, :x) * scalar(cache, :z)
  end

  @testset "Projections" begin
    @test project!(scalar(cache, :x), 1) == scalar(cache, 0)
    @test project!(scalar(cache, :x), 0) == scalar(cache, :x)
    @test project!(blade(cache, 1, 2), 1) == scalar(cache, 0)
    @test project!(blade(cache, 1, 2) + blade(cache, 1), 1) == blade(cache, 1)
    @test project!(blade(cache, 1) + blade(cache, 1, 2) + blade(cache, 1, 2, 3), 2) == blade(cache, 1, 2)
  end

  @testset "Reversions" begin
    @test reverse(blade(cache, 1, 2)) == blade(cache, 2, 1)
    @test reverse(scalar(cache, :x) * blade(cache, 1, 2)) == scalar(cache, :x) * blade(cache, 2, 1)
    @test reverse(blade(cache, 1, 2, 3) + blade(cache, 2) + blade(cache, 2, 3)) == blade(cache, 3, 2, 1) + blade(cache, 2) + blade(cache, 3, 2)

    @test antireverse(blade(cache, 1, 2)) == blade(cache, 2, 1)
    @test antireverse(scalar(cache, :x) * blade(cache, 1, 2)) == scalar(cache, :x) * blade(cache, 2, 1)
    @test antireverse(blade(cache, 1, 2, 3) + blade(cache, 2) + blade(cache, 2, 3)) == blade(cache, 1, 2, 3) - blade(cache, 2) + blade(cache, 3, 2)
    _cache = ExpressionCache(Signature(3, 0, 1))
    @test antireverse(weighted(blade(_cache, 4), 1)) == antireverse(blade(_cache, 4)) == -blade(_cache, 4)
  end

  @testset "Exterior products" begin
    x = blade(cache, 1, 2)
    y = blade(cache, 3)

    @test exterior_product(blade(cache, 1, 2), blade(cache, 3)) == blade(cache, 1, 2, 3)
    @test exterior_product(blade(cache, 1, 2), blade(cache, 2)) == scalar(cache, 0)
  end

  @testset "Common operators" begin
    @test Expression(cache, INTERIOR_PRODUCT, blade(cache, 1), blade(cache, 1, 2)) == blade(cache, 2)
    @test Expression(cache, COMMUTATOR_PRODUCT, blade(cache, 1), blade(cache, 2)) == weighted(blade(cache, 1, 2), 1.0)
    @test Expression(cache, EXTERIOR_PRODUCT, blade(cache, 1), blade(cache, 2)) == blade(cache, 1, 2)
  end

  @testset "Inversion" begin
    @test Expression(cache, INVERSE, scalar(cache, 2.0)) == scalar(cache, 0.5)
    @test Expression(cache, INVERSE, scalar(cache, :x)) == scalar(cache, :($inv(x)))
    @test Expression(cache, INVERSE, scalar(cache, 2.0)) == scalar(cache, 0.5)
    @test Expression(cache, INVERSE, blade(cache, 1, 2)) == weighted(blade(cache, 1, 2), -1)
    @test Expression(cache, INVERSE, weighted(blade(cache, 1, 2), 5.0)) == weighted(blade(cache, 1, 2), -0.2)
    versor = Expression(cache, ADDITION, scalar(cache, 2.0), weighted(blade(cache, 1, 2), 5.0))
    @test Expression(cache, GEOMETRIC_PRODUCT, versor, Expression(cache, INVERSE, versor)) == scalar(cache, 1.0)
    mv = Expression(cache, ADDITION, scalar(cache, 2.0), weighted(blade(cache, 3), 1.2), weighted(blade(cache, 1, 2), 5.0))
    @test_throws "only supported for versors" Expression(cache, GEOMETRIC_PRODUCT, mv, Expression(cache, INVERSE, mv)) == scalar(cache, 1.0)
  end

  @testset "Exponentiation" begin
    _cache = ExpressionCache(Signature(3, 0, 1))
    b = blade(_cache, 1, 2, 4)
    ex = Expression(_cache, EXPONENTIAL, b)
    @test ex == Expression(_cache, ADDITION, scalar(_cache, 1), b)

    _cache = ExpressionCache(Signature(3))
    for α in (1, 3.2)
      b = weighted(blade(_cache, 1, 2), α)
      ex = Expression(_cache, EXPONENTIAL, b)
      @test grade(ex) == [0, 2]
      @test ex == Expression(_cache, ADDITION, scalar(_cache, cos(α)), Expression(_cache, GEOMETRIC_PRODUCT, scalar(_cache, sin(α) / α), b))
    end
    α = :α
    b = weighted(blade(_cache, 1, 2), α)
    ex = Expression(_cache, EXPONENTIAL, b)
    @test isa(ex, Expression)
    @macroexpand @ga (3, 0, 1) exp(α::e12)
  end
end;
