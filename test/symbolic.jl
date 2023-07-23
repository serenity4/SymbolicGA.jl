using SymbolicUtils

@testset "Symbolic expressions" begin
  @syms x₁ x₂ x₃ y₁ y₂ y₃ z₁ z₂ z₃
  x = (x₁, x₂, x₃)
  y = (y₁, y₂, y₃)
  z = (z₁, z₂, z₃)
  det = @ga 3 x::1 ∧ y::1 ∧ z::1
  @test isequal(det[], x₁*y₂*z₃ + x₂*y₃*z₁ + x₃*y₁*z₂ - x₂*y₁*z₃ - x₁*y₃*z₂ - x₃*y₂*z₁)

  @syms cgx cgy cgz cgw cvx cvy cvz cmx cmy cmz
  @syms ogx ogy ogz ogw ovx ovy ovz omx omy omz
  c = @cga3 (cgx::e423 + cgy::e431 + cgz::e412 + cgw::e321 + cvx::e415 + cvy::e425 + cvz::e435 + cmx::e235 + cmy::e315 + cmz::e125)
  o = @cga3 (ogx::e423 + ogy::e431 + ogz::e412 + ogw::e321 + ovx::e415 + ovy::e425 + ovz::e435 + omx::e235 + omy::e315 + omz::e125)
  p = @cga3 c::3 ∨ o::3
  @test isequal(collect(p)[1:5], [
    cgz*omy - cgy*omz + cmy*ogz - cmz*ogy + cvx*ogw + cgw*ovx,
    cgx*omz - cgz*omx + cmz*ogx - cmx*ogz + cvy*ogw + cgw*ovy,
    cgy*omx - cgx*omy + cmx*ogy - cmy*ogx + cvz*ogw + cgw*ovz,
    -(cgx*ovx + cgy*ovy + cgz*ovz + cvx*ogx + cvy*ogy + cvz*ogz),
    -(cmx*ovx + cmy*ovy + cmz*ovz + cvx*omx + cvy*omy + cvz*omz),
  ])
end;
