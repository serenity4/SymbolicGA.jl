using LazyGeometricAlgebra: extract_weights, input_expression, extract_expression, restructure, expand_variables, traverse

function traverse_collect(f, x, T = Expression)
  res = []
  traverse(x, T) do y
    f(y) === true && push!(res, y)
    nothing
  end
  res
end

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

    ex = quote
      n = 1.0::e4 + 1.0::e5
      magnitude2(x) = x ⦿ x
      weight(X) = (-X ⋅ n)::Scalar
      normalize(X) = X / weight(X)
      radius2(X) = (magnitude2(X) / magnitude2(X ∧ n))::Scalar
      radius(X) = normalize(radius2(X))::Scalar
      radius(S::Quadvector)
    end
    ex2 = expand_variables(ex)
    symbols = traverse_collect(ex -> in(ex, (:radius, :radius2, :normalize, :weight, :magnitude2, :n)), ex2, Expr)
    @test isempty(symbols)
  end

  sig = Signature(1, 1, 1)
  ws = extract_weights(sig, :x, 1, 0)
  @test ws == [:($getcomponent(x, $i)) for i in 1:3]
  ex = input_expression(sig, :x, 2)
  @test isexpr(ex, :+, 3)
  @test ex[1] == scalar(first(ws)) * blade(1, 2)

  ex = extract_expression(:((x::Vector * y::Bivector)::Trivector), sig)
  ex2 = restructure(ex, sig)
  @test isexpr(ex2, :kvector, 1)
  @test isweighted(ex2[1]) && isexpr(ex2[1][2], :blade)
  @test string(ex2) == "kvector₃((x[1] * y[3] + x[2] * y[2] * -1 + x[3] * y[1]) ⟑ e₁₂₃)"

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
  res = @ga 3 x::1 * y::KVector{1}
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
  res = @ga 3 (1::e1 * 1::e1 + 1.0::e12)::Multivector
  @test res == (1, 1.0)

  res = @ga 3 (1::e1 * 1::e1 + 2::e12)::Multivector
  @test res == (1, 2)

  res = @ga 3 ((x::Vector)')
  @test res == x
  res = @ga 3 ((x::Bivector)')
  @test res == (-).(x)

  x = (1, 2, 3)
  @test_broken @macroexpand (@ga 3 (x::Vector * x::Bivector ∧ x::Vector + 2::e12)::Multivector) isa Expr

  @test (@ga 3 dual(1::e2)) == (@ga 3 1::e31)

  y = (101, 102, 103)
  @test (@ga 3 (x::1 × y::1)::2) == (@ga 3 (x::1 ∧ y::1))
end;
