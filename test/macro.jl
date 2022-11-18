using LazyGeometricAlgebra: extract_weights, kvector_expression, extract_base_expression, expand_variables

@testset "Macro frontend" begin
  @testset "Function and variable expansion" begin
    # Recursive reference.
    ex = quote
      x = x::Vector
      x * x
    end
    @test expand_variables(ex) == :(x::Vector * x::Vector)

    # Interleaved references/function calls.
    ex = quote
      b1 = 1::e1
      g(z) = z + b1
      y = (1, 2, 3)
      x = g(y::Vector)
      x
    end
    ex2 = expand_variables(ex)
    @test ex2 == :((1, 2, 3)::Vector + 1::e1)
  end

  s = Signature(1, 1, 1)
  ws = extract_weights(s, :x, 1, 0)
  @test ws == [:($getcomponent(x, $i)) for i in 1:3]
  kvec = kvector_expression(s, :x, 2)
  @test isexpr(kvec, :kvector, 3)
  @test kvec[1] == weighted(blade(1, 2), first(ws))

  ex = extract_base_expression(:((x::Vector * y::Bivector)::Trivector), s)
  ex2 = simplify(ex, s)
  @test isexpr(ex2, :kvector, 1)
  @test isweighted(ex2[1]) && isexpr(ex2[1][2], :blade)
  @test string(ex2) == "kvector₃((x[1] * y[3] + x[2] * y[2] * -1 + x[3] * y[1]) * e₁₂₃)"

  ex = @macroexpand @ga (2, 1) x::Vector ∧ y::Vector + x::Vector * z::Pseudoscalar
  @test isa(ex, Expr)
  x = (1, 2, 3)
  y = (4, 5, 6)
  z = 3

  # Yields 1 bivector.
  res = @ga (2, 1) x::Vector ∧ y::Vector + x::Vector * z::Pseudoscalar
  @test isa(res, NTuple{3,Int})

  x = (1, 2)
  y = (0, 50)
  res = @ga 2 x::Vector ∧ y::Vector + x[1]::Scalar * z::Pseudoscalar
  @test res == 1 * 50 + 1 * 3
  # Yields 1 pseudoscalar and 1 vector.
  res = @ga 2 x::Vector ∧ y::Vector + x::Vector * z::Pseudoscalar
  @test isa(res, NTuple{3, Int})
  res2 = @ga Vector 2 x::Vector ∧ y::Vector + x::Vector * z::Pseudoscalar
  @test collect(res) == res2

  x = (1.0, 2.0, 3.0)
  y = (50.0, 70.0, 70.0)
  # Yields 1 scalar and 1 bivector.
  res = @ga 3 x::Vector * y::Vector
  @test isa(res, NTuple{4,Float64})
  @test res[1] == sum(x .* y)

  res = @ga 3 begin
    x::Vector
    x * x
  end
  @test isa(res, NTuple{4, Float64})

  res2 = @ga 3 begin
    x = (1.0, 2.0, 3.0)::Vector
    x * x
  end
  @test res === res2

  # The `1::e12` gets simplified to `e12`.
  res = @ga 3 begin
    (1::e1 * 1::e1 + 1::e12)::Multivector
  end
  @test res == (1, 1)

  # Preserve element types.
  res = @ga 3 begin
    (1::e1 * 1::e1 + 1.0::e12)::Multivector
  end
  @test res == (1, 1.0)

  res = @ga 3 begin
    (1::e1 * 1::e1 + 2::e12)::Multivector
  end
  @test res == (1, 2)

  x = (1, 2, 3)
  @test_broken @macroexpand @ga 3 begin
    (x::Vector * x::Bivector ∧ x::Vector + 2::e12)::Multivector
  end isa Expr
end;
