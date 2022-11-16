@testset "Macro frontend" begin
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
end;
