using SymbolicUtils

@testset "Symbolic expressions" begin
  @syms x₁ x₂ x₃ y₁ y₂ y₃ z₁ z₂ z₃
  x = (x₁, x₂, x₃)
  y = (y₁, y₂, y₃)
  z = (z₁, z₂, z₃)
  det = @ga 3 x::1 ∧ y::1 ∧ z::1
  @test isequal(det[], x₁*y₂*z₃ + x₂*y₃*z₁ + x₃*y₁*z₂ - x₂*y₁*z₃ - x₁*y₃*z₂ - x₃*y₂*z₁)
end;
