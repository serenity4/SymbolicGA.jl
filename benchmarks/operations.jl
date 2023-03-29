using SymbolicGA, LinearAlgebra, StaticArrays, BenchmarkTools

x = (1.0, 2.0, 3.0)
y = (50.0, 70.0, 70.0)

f(x, y) = @ga 3 x::Vector ⟑ y::Vector
@macroexpand @ga 3 x::Vector ⟑ y::Vector
@btime f($x, $y)
@code_typed f(x, y)

# Determinant - rank 2

mydet(A₁, A₂) = @ga(2, A₁::Vector ∧ A₂::Vector)[]
A₁ = @SVector rand(2)
A₂ = @SVector rand(2)
A = SMatrix([A₁ A₂])
@assert mydet(A₁, A₂) ≈ det(A)

@btime det($A)
@btime mydet($A₁, $A₂)
# `Base.sub_float` is probably faster than `Base.mul_float` with -1 and then `Base.add_float`,
# which would explain why `det(A)` is slightly faster.
@code_warntype optimize=true det(A)
@code_warntype optimize=true mydet(A₁, A₂)

# Determinant - rank 4

mydet(A₁, A₂, A₃, A₄) = @ga(4, A₁::Vector ∧ A₂::Vector ∧ A₃::Vector ∧ A₄::Vector)[]
@macroexpand @ga(4, A₁::Vector ∧ A₂::Vector ∧ A₃::Vector ∧ A₄::Vector)
A₁ = @SVector rand(4)
A₂ = @SVector rand(4)
A₃ = @SVector rand(4)
A₄ = @SVector rand(4)
A = SMatrix([A₁ A₂ A₃ A₄])
@assert mydet(A₁, A₂, A₃, A₄) ≈ det(A)

@btime det($A)
@btime mydet($A₁, $A₂, $A₃, $A₄)
@code_warntype optimize=true det(A)
@code_warntype optimize=true mydet(A₁, A₂, A₃, A₄)

# Rotations - 3D

function rot(a, b, x, α)
  @ga 3 begin
    # Define unit plane for the rotation.
    Π = a::Vector ⟑ b::Vector
    # Define rotation generator.
    Ω = exp((-α::Scalar / 2::Scalar) ⟑ Π)
    # Apply the rotation by sandwiching x with Ω.
    Ω ⟑ x::Vector ⟑ reverse(Ω)
  end
end

a = (1.0, 0.0, 0.0)
b = (0.0, 1.0, 0.0)
x = (1.0, 1.0, 0.0)
α = π / 4

x′ = rot(a, b, x, α)
@assert x′ ≈ KVector{1,3}(0.0, sqrt(2), 0.0)
@btime rot($a, $b, $x, $α)
@code_warntype rot(a, b, x, α)

@time @macroexpand @ga 3 begin
  # Define unit plane for the rotation.
  Π = a::Vector ⟑ b::Vector
  # Define rotation generator.
  Ω = exp((-α::Scalar / 2::Scalar) ⟑ Π)
  # Apply the rotation by sandwhiching x with Ω.
  Ω ⟑ x::Vector ⟑ reverse(Ω)
end;

@time @macroexpand @ga 3 begin
  Π = a::Vector ⟑ b::Vector
  unitize(Π)
end;
