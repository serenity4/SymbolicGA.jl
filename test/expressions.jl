using SymbolicGA: infer_grade, project!, dereference

sig = Signature(3, 1)
cache = ExpressionCache(sig)
sc(x) = scalar(cache, x)
fac(x) = factor(cache, x)
bl(args...) = blade(cache, args...)
x = sc(:x)
y = sc(:y)
e1, e2, e3, e4 = blade.(cache, [1, 2, 3, 4])

@testset "Expressions" begin
  @testset "Expression basics" begin
    @test isexpr(fac(0), FACTOR, 1)
    @test !isexpr(bl(1, 2), BLADE, 1)
    @test isexpr(bl(1, 2), BLADE, 2)
    @test isexpr(bl(1, 2), (FACTOR, BLADE))

    ex = bl(1, 2)
    ex2 = postwalk(x -> isa(dereference(cache, x), Int) ? dereference(cache, x) + 1 : x, ex)
    @test ex2 == bl(2, 3)
  end

  @testset "Grade inference" begin
    @test infer_grade(cache, FACTOR, 0) == 0
    @test infer_grade(cache, BLADE, [1, 2, 3]) == 3
    @test infer_grade(cache, BLADE, [1, 2, 3, 3]) == 2
    @test infer_grade(cache, BLADE, [1, 2, 3, 3, 3]) == 3
    @test infer_grade(cache, BLADE, [1, 1, 2, 2, 3, 3, 3]) == 1
    @test infer_grade(cache, BLADE, [1, 2, 3, 1, 2, 4, 1, 2]) == 4
    @test infer_grade(cache, KVECTOR, [bl(1, 2), bl(2, 3)]) == 2
    @test infer_grade(cache, MULTIVECTOR, [bl(1, 2), bl(2, 3)]) == 2
    @test infer_grade(cache, MULTIVECTOR, [kvector(bl(1, 2), bl(2, 3)), kvector(bl(1))]) == [1, 2]

    _cache = ExpressionCache(Signature(3))
    ex = blade(_cache, 1, 2)
    @test grade(ex) == 2
    @test antigrade(ex) == 1
    ex = blade(_cache, 1, 2) + blade(_cache, 1)
    @test grade(ex) == [1, 2]
    @test antigrade(ex) == [2, 1]
  end

  @testset "Blades and metric simplifications" begin
    ex = bl(1, 1)
    @test ex.grade == 0
    @test ex == sc(1)

    ex = bl(1, 2, 3, 1)
    @test ex.grade == 2
    @test ex == bl(2, 3)

    ex = bl(4, 4)
    @test ex.grade == 0
    @test ex == sc(-1)

    ex = bl(1, 2, 1)
    @test ex.grade == 1
    @test ex == weighted(bl(2), -1)

    @test bl(1) * bl(2) == bl(1, 2)
    @test bl(1, 2) * bl(3) == bl(1, 2, 3)
  end

  @testset "Simplification of null elements in addition" begin
    @test fac(1) + fac(0) == fac(1)
    @test bl(1) + fac(0) == bl(1)
  end

  @testset "Disassociation of products and sums" begin
    @test fac(1) * (fac(:x) * fac(:y)) == fac(:(x * y))
    @test fac(:z) * (fac(:x) * fac(:y)) == fac(:(z * x * y))
    @test fac(1) + (fac(:x) + fac(:y)) == fac(:(x + y + 1))
    @test fac(1) + (fac(2) + fac(0)) == fac(3)
  end

  @testset "Distribution of products" begin
    @test (fac(:x) + fac(:y)) * (fac(:w) + fac(:z)) == fac(:((x + y) * (w + z)))
    @test (bl(1) + bl(3)) * (bl(2) + bl(4)) == bl(1, 2) + bl(1, 4) + bl(3, 2) + bl(3, 4)
    @test (fac(:x) + bl(1)) * (fac(:y) + bl(4)) == fac(:(x * y)) + fac(:x) * bl(4) + bl(1) * fac(:y) + bl(1, 4)
    @test (fac(:x) + fac(:y)) * bl(1, 2) == (fac(:x) + fac(:y)) ⟑ bl(1, 2)
  end

  @testset "Simplification and canonicalization of factors" begin
    @test bl(1, 2) * fac(3) == fac(3) ⟑ bl(1, 2)
    @test bl(1, 2) * fac(3) * fac(5) == fac(15) ⟑ bl(1, 2)
    @test bl(1, 2) * fac(:x) * fac(:(y[1])) == fac(:(x * y[1])) ⟑ bl(1, 2)
    @test fac(1) * bl(1, 2) * fac(3) == fac(3) ⟑ bl(1, 2)
    # @test bl(1, 2) * fac(0) == Expression(GEOMETRIC_PRODUCT, fac(0), bl(1, 2); simplify = false)
    @test bl(1, 2) * fac(0) == fac(0)
    @test fac(:(-1 * -1)) == fac(1)
    @test fac(:(-1 * -1)) * bl(1, 2) == bl(1, 2)

    @testset "Simplification of additive factors" begin
      @test fac(:x) + fac(:y) == fac(:(x + y))
      @test fac(:x) + fac(1) == fac(:(x + 1))
      @test fac(2) + fac(3) == fac(5)
      @test fac(:x) + fac(3) + fac(:y) + fac(2) == fac(:(x + y + 5))

      @test x - x == fac(0)
      @test x + x == sc(:(2x))
      @test x + y == sc(:(x + y))
      @test x + x - sc(:(2x)) == fac(0)
      @test sc(:(x * y)) - sc(:(x * y)) == fac(0)
      @test sc(:(x * y)) + sc(:(x * y)) ≠ fac(0)
      @test x - sc(:(2x)) == sc(:(-1 * x))
    end
  end

  @testset "Blade grouping over addition" begin
    x_e13 = weighted(bl(1, 3), :x)
    ex = x_e13 + x_e13
    @test ex == weighted(bl(1, 3), :(2x))

    ex = x + y
    @test ex == sc(:(x + y))

    ex = weighted(bl(1), :x) + weighted(bl(2, 3), :y) + weighted(bl(1), :z)
    @test ex == weighted(bl(2, 3), :y) + weighted(bl(1), :(x + z))
  end

  @testset "Projections" begin
    @test project!(fac(:x), 1) == fac(0)
    @test project!(fac(:x), 0) == fac(:x)
    @test project!(bl(1, 2), 1) == fac(0)
    @test project!(bl(1, 2) + bl(1), 1) == bl(1)
    @test project!(bl(1) + bl(1, 2) + bl(1, 2, 3), 2) == bl(1, 2)
  end

  @testset "Reversions" begin
    @test reverse(bl(1, 2)) == bl(2, 1)
    @test reverse(fac(:x) * bl(1, 2)) == fac(:x) * bl(2, 1)
    @test reverse(bl(1, 2, 3) + bl(2) + bl(2, 3)) == bl(3, 2, 1) + bl(2) + bl(3, 2)

    @test antireverse(bl(1, 2)) == bl(2, 1)
    @test antireverse(fac(:x) * bl(1, 2)) == fac(:x) * bl(2, 1)
    @test antireverse(bl(1, 2, 3) + bl(2) + bl(2, 3)) == bl(1, 2, 3) - bl(2) + bl(3, 2)
    _cache = ExpressionCache(Signature(3, 0, 1))
    @test antireverse(weighted(blade(_cache, 4), 1)) == antireverse(blade(_cache, 4)) == -blade(_cache, 4)
  end

  @testset "Exterior products" begin
    x = bl(1, 2)
    y = bl(3)

    @test exterior_product(bl(1, 2), bl(3)) == bl(1, 2, 3)
    @test exterior_product(bl(1, 2), bl(2)) == fac(0)
  end

  @testset "Common operators" begin
    @test Expression(cache, INTERIOR_PRODUCT, bl(1), bl(1, 2)) == bl(2)
    @test Expression(cache, COMMUTATOR_PRODUCT, bl(1), bl(2)) == weighted(bl(1, 2), 1.0)
    @test Expression(cache, EXTERIOR_PRODUCT, bl(1), bl(2)) == bl(1, 2)
  end

  @testset "Inversion" begin
    @test Expression(cache, INVERSE, fac(2.0)) == fac(0.5)
    @test Expression(cache, INVERSE, fac(:x)) == fac(:($inv(x)))
    @test Expression(cache, INVERSE, sc(2.0)) == sc(0.5)
    @test Expression(cache, INVERSE, bl(1, 2)) == weighted(bl(1, 2), -1)
    @test Expression(cache, INVERSE, weighted(bl(1, 2), 5.0)) == weighted(bl(1, 2), -0.2)
    versor = Expression(cache, ADDITION, sc(2.0), weighted(bl(1, 2), 5.0))
    @test Expression(cache, GEOMETRIC_PRODUCT, versor, Expression(cache, INVERSE, versor)) == sc(1.0)
    mv = Expression(cache, ADDITION, sc(2.0), weighted(bl(3), 1.2), weighted(bl(1, 2), 5.0))
    @test_throws "only supported for versors" Expression(cache, GEOMETRIC_PRODUCT, mv, Expression(cache, INVERSE, mv)) == sc(1.0)
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
  end
end;
