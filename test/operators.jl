@testset "Jacobi identity" begin
  lhs = @ga Vector 4 begin
    A = 1.3::e1 + 2.7::e12
    B = 0.7::e3 + 1.1::e123 + 3.05::e
    C = 0.7::e4 + 0.1::e2 + 1.59::e23 + 1.1::e124 + 3.05::e1234
    A × (B × C)
  end

  rhs = @ga Vector 4 begin
    A = 1.3::e1 + 2.7::e12
    B = 0.7::e3 + 1.1::e123 + 3.05::e
    C = 0.7::e4 + 0.1::e2 + 1.59::e23 + 1.1::e124 + 3.05::e1234
    -(B × (C × A) + C × (A × B))
  end

  @test_broken lhs ≈ rhs
end;
