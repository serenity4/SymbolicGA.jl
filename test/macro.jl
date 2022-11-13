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
  @test kvec.args[1] == weighted(first(blades), first(ws))
end;
