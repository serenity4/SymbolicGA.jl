@testset "Jacobi identity" begin
  A = @ga 4 (1.3::e1 + 2.7::e12)
  B = @ga 4 0.7::e3 + 1.1::e123 + 3.05::e
  C = @ga 4 0.7::e4 + 0.1::e2 + 1.59::e23 + 1.1::e124 + 3.05::e1234

  # @ga A × (B × C) ≈ -(B × (C × A) + C × (A × B))
end
