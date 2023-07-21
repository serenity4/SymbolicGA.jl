point(A) = @cga3 point(A)
point_pair(A, B) = @cga3 point(A) ∧ point(B)
circle(A, B, C) = @cga3 point(A) ∧ point(B) ∧ point(C)
line(A, B) = @cga3 point(A) ∧ point(B) ∧ n
sphere(A, B, C, D) = @cga3 point(A) ∧ point(B) ∧ point(C) ∧ point(D)
plane(A, B, C) = @cga3 point(A) ∧ point(B) ∧ point(C) ∧ n
circle_radius(X) = sqrt(@cga3(Float64, radius2(X::Trivector)))
sphere_radius(X) = sqrt(@cga3(Float64, radius2(X::Quadvector)))

@testset "3D Conformal Geometric Algebra" begin
  isnullvector(X) = isapprox(@cga3(Float64, magnitude2(X::Vector)), 0; atol = 1e-14)
  @test (@cga3 n ⋅ n̄) == (@cga3 n̄ ⋅ n) == KVector{0,5}((-1.0,))
  @test (@cga3 magnitude2(n ⦿ n)) == (@cga3 magnitude2(n̄ ⦿ n̄)) == KVector{0,5}((0,))
  A = sqrt(2) .* (1, 0, 0)
  B = sqrt(2) .* (0, 1, 0)
  C = sqrt(2) .* (0, 0, 1)
  D = sqrt(2) .* (-1, 0, 0)
  @test all(isnullvector, point.((A, B, C, D)))
  S1 = sphere(A, B, C, D)
  𝒜 = point(A)
  @test (@cga3 unitize(($(𝒜 .* 2))::Vector)) == 𝒜
  O = (0, 0, 0)
  C1 = @cga3 center(S1::Quadvector)
  @test @cga3(unitize(C1::Vector)) ≈ point(O)
  @test length(@cga3 weight(S1::Quadvector)) == 10
  @test @cga3(Float64, radius2(S1::Quadvector)) ≈ sphere_radius(S1)^2 ≈ 2.0
end;

struct Camera{T}
  optical_center::KVector{1,T,4,4} # ::Vector
  image_plane::KVector{3,T,4,4} # ::Trivector
end
Camera(A₁, A₂, A₃, A₄) = Camera(@pga3(point(A₄)), @pga3 point(A₁) ∧ point(A₂) ∧ point(A₃))

project_point(camera::Camera, x) = @pga3 begin
  line = camera.optical_center::Vector ∧ point(x)
  line ∨ camera.image_plane::Trivector
end

@testset "3D Projective Geometric Algebra" begin
  A = @pga3 point((1, 0, 0))
  B = @pga3 point((0, 1, 0))
  C = @pga3 point((1, 1, 0))
  D = @pga3 point((0, 0, 1))

  image_plane = @pga3 A::Vector ∧ B::Vector ∧ C::Vector
  optical_center = D

  camera = Camera(optical_center, image_plane)
  p = project_point(camera, (1.2, 1, 0))
  @test (@pga3 unitize(p::Vector)) == KVector{1,4}(-1.2, -1, 0, -1)
  p = project_point(camera, (1.2, 1, 2))
  @test (@pga3 unitize(p::Vector)) == KVector{1,4}(-1.2, -1, 0, 1)
  # The above operation is equivalent to an inversion through the optical center, as the original point is exactly at a distance of one focal length.
  @test (@pga3 unitize(−D::Vector ⩒ point((1.2, 1, 2)) ⩒ antireverse(D::Vector))) == KVector{1,4}(1.2, 1, 0, -1)
end

count_expr_nodes(ex) = isa(ex, Expr) ? sum(count_expr_nodes, ex.args) : 1

@testset "3D rotations" begin
  function rotate_3d(x, a, b, α)
    # Define a unit plane for the rotation.
    # The unitization ensures we don't need `a` and `b` to be orthogonal nor to be unit vectors.
    Π = @ga 3 unitize(a::1 ∧ b::1)

    # Define rotation generator.
    Ω = @ga 3 exp(-(0.5α)::0 ⟑ Π::2)
    # Apply the rotation with the versor product of x by Ω.
    @ga 3 x::1 << Ω::(0, 2)
  end

  a = (1.0, 0.0, 0.0)
  b = (0.0, 1.0, 0.0)
  x = (1.0, 1.0, 0.0)
  α = π / 4

  # Define a plane for the rotation.
  Π = @ga 3 a::Vector ∧ b::Vector

  # Define rotation bivector.
  ϕ = @ga 3 α::Scalar ⟑ Π::Bivector

  # Define rotation generator.
  Ω = @ga 3 exp(-(ϕ::Bivector) / 2::Scalar)
  @test grade.(Ω) == (0, 2)
  @test all(Ω .≈ @ga 3 $(cos(0.5α))::Scalar - Π::Bivector ⟑ $(sin(0.5α))::Scalar)
  @test (@ga 3 Ω::(Scalar, Bivector) ⟑ inverse(Ω::(Scalar, Bivector))) == KVector{0,3}(1.0)

  x′ = @ga 3 x::1 << Ω::(0, 2)
  @test x′ ≈ KVector{1,3}(0.0, sqrt(2), 0.0)

  @test rotate_3d((1.0, 0.0, 0.0), a, b, π/6) ≈ KVector{1,3}(cos(π/6), sin(π/6), 0.0)
  @test rotate_3d((1.0, 0.0, 0.0), a, b, π/3) ≈ KVector{1,3}(cos(π/3), sin(π/3), 0.0)
  @test rotate_3d((2.0, 0.0, 0.0), a, b, π/3) ≈ KVector{1,3}(2cos(π/3), 2sin(π/3), 0.0)
  @test rotate_3d((2.0, 0.0, 0.0), a, 2 .* b, π/3) ≈ rotate_3d((2.0, 0.0, 0.0), a, b, π/3)
  @test rotate_3d((2.0, 0.0, 0.0), a, (1/sqrt(2), 1/sqrt(2), 0.0), π/3) ≈ rotate_3d((2.0, 0.0, 0.0), a, b, π/3)
  @test rotate_3d((2.0, 0.0, 0.0), a, (1.0, 1.0, 0.0), π/3) ≈ rotate_3d((2.0, 0.0, 0.0), a, b, π/3)

  # Do it more succinctly.
  ex = @macroexpand @ga 3 begin
    Π = a::1 ⟑ b::1
    Ω = exp((-(0.5α)::0) ⟑ Π)
  end;
  @test_broken count_expr_nodes(ex) < 1000

  @test_skip begin
    x′′ = @ga 3 begin
      # Define unit plane for the rotation.
      Π = a::1 ⟑ b::1
      # Define rotation generator.
      Ω = exp((-(0.5α)::0) ⟑ Π)
      # Apply the rotation by sandwiching x with Ω.
      Ω ⟑ x::1 ⟑ reverse(Ω)
    end
    x′′ === x′
  end
end;

@testset "Oriented 3D CGA" begin
  @testset "Inclusion of a point in a line segment" begin
    A = rand(3)
    B = rand(3)
    L = @cga3 point(A) ∧ point(B) ∧ n
    P = A .+ 0.5 .* (B .- A)

    function line_tests(P)
      t₁ = @cga3 begin
        L = point(A) ∧ point(B) ∧ n
        (point(A) ∧ point(P) ∧ n) ⟑ L
      end
      t₂ = @cga3 begin
        L = point(A) ∧ point(B) ∧ n
        (point(P) ∧ point(B) ∧ n) ⟑ L
      end
      (t₁, t₂)
    end
    PRECISION = 1e-15
    is_zero(x, y) = isapprox(x, y; atol = PRECISION)
    is_positive(x::Number) = x ≥ -PRECISION
    is_zero_bivector(x) = is_zero(x, zero(KVector{2,Float64,5}))
    is_on_line((t₁, t₂)) = is_zero_bivector(t₁[2]) && is_zero_bivector(t₂[2])
    is_within_segment((t₁, t₂)) = is_positive(t₁[1][]) && is_positive(t₂[1][])

    ret = line_tests(A)
    t₁, t₂ = ret
    @test is_on_line(ret)
    @test is_within_segment(ret)
    @test isapprox(t₁[1][], 0.0; atol = 1e-15) && t₂[1][] ≥ -1e-15

    ret = line_tests(B)
    t₁, t₂ = ret
    @test is_on_line(ret)
    @test is_within_segment(ret)
    @test t₁[1][] ≥ -1e-15 && isapprox(t₂[1][], 0.0; atol = 1e-15)

    ret = line_tests(A .+ 0.5 .* (B .- A))
    @test is_on_line(ret)
    @test is_within_segment(ret)

    ret = line_tests(A .+ -0.1 .* (B .- A))
    @test is_on_line(ret)
    @test !is_within_segment(ret)

    ret = line_tests(A .+ 1.1 .* (B .- A))
    @test is_on_line(ret)
    @test !is_within_segment(ret)

    ret = line_tests((100.0, 100.0, 100.0))
    @test !is_on_line(ret)
    @test !is_within_segment(ret)
  end
end;
