@testset "Macro frontend" begin
  s = Signature(1, 1, 1)
  blades = blade_expressions(s, 1)
  @test all(isexpr(blade, :blade, 1) for blade in blades)
  blades = blade_expressions(s, 2)
  @test all(isexpr(blade, :blade, 2) for blade in blades)
  ws = extract_weights(s, :x, 1, 0)
  @test ws == [:(getcomponent(x, $i)) for i in 1:3]
  kvec = kvector_expression(s, :x, 2)
  @test isexpr(kvec, :kvector, 3)
  @test kvec[1] == weighted(first(blades), first(ws))

  ex = extract_base_expression(:((x::Vector * y::Bivector)::Trivector), s)
  ex2 = simplify(ex, s)
  @test isexpr(ex2, :kvector, 1)
  @test isweighted(ex2[1]) && isexpr(ex2[1][2], :blade)
  @test string(ex2) == "kvector₃((x[1] * y[3] + x[2] * y[2] * -1 + x[3] * y[1]) * e₁₂₃)"
end;
