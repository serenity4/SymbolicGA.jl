macro cga3(args...)
  definitions = quote
    n = 1.0::e4 + 1.0::e5
    n̄ = (-0.5)::e4 + 0.5::e5
    embed(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
    magnitude2(x) = x ⦿ x
    weight(X) = -X ⋅ n
    unitize(X) = X / weight(X)
    radius2(X) = (magnitude2(X) / magnitude2(X ∧ n))::Scalar
    center(X) = X * n * X
    # For spheres `S` defined as vectors, and points `X` defined as vectors as well.
    distance(S, X) = unitize(S) ⋅ unitize(X)
  end
  varinfo = parse_variable_info(definitions; warn_override = false)
  esc(codegen_expression((4, 1, 0), args...; varinfo))
end

point(x) = @cga3 (embed(x) + (0.5::Scalar * magnitude2(embed(x))) * n + n̄)::Vector
point_pair(A, B) = @cga3 A::Vector ∧ B::Vector
circle(A, B, C) = @cga3 point_pair(A, B)::Bivector ∧ C::Vector
line(A, B, C) = @cga3 point_pair(A, B)::Bivector ∧ n
sphere(A, B, C, D) = @cga3 circle(A, B, C)::Trivector ∧ D::Vector
plane(A, B, C) = @cga3 circle(A, B, C)::Trivector ∧ n
circle_radius(X) = sqrt(-@cga3(radius2(X::Trivector))[])
sphere_radius(X) = sqrt(@cga3(radius2(X::Quadvector))[])

≊(a, b) = all(isapprox.(a, b; atol = 1e-15))

@testset "3D Conformal Geometric Algebra" begin
  isnullvector(X) = iszero(@cga3(magnitude2(X::Vector))[])
  @test (@cga3 n ⋅ n̄) == (@cga3 n̄ ⋅ n) == KVector{0,5}((-1.0,))
  @test (@cga3 magnitude2(n ⦿ n)) == (@cga3 magnitude2(n̄ ⦿ n̄)) == KVector{0,5}((0,))
  A = point(sqrt(2) .* (1, 0, 0))
  B = point(sqrt(2) .* (0, 1, 0))
  C = point(sqrt(2) .* (0, 0, 1))
  D = point(sqrt(2) .* (-1, 0, 0))
  @test all(isnullvector, (A, B, C, D))
  S1 = sphere(A, B, C, D)
  @test (@cga3 unitize((A .* 2)::Vector)) == A
  O = point((0, 0, 0))
  C1 = @cga3 center(S1::Quadvector)::Vector
  @test @cga3(unitize(C1::Vector)) ≊ O
  @test length(@cga3 weight(S1::Quadvector)) == 10
  @test (@cga3(radius2(S1::Quadvector))[]) ≈ sphere_radius(S1)^2 ≈ 2.0
end;

macro pga3(args...)
  definitions = quote
    embed(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
    magnitude2(x) = x ⦿ x
    point(x) = embed(x) + 1.0::e4
  end
  varinfo = parse_variable_info(definitions; warn_override = false)
  esc(codegen_expression((3, 0, 1), args...; varinfo))
end

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

@testset "3D rotations" begin
  a = (1.0, 0.0, 0.0)
  b = (0.0, 1.0, 0.0)
  x = (1.0, 1.0, 0.0)
  α = π / 4

  # Define a plane for the rotation.
  Π = @ga 3 a::Vector ∧ b::Vector

  # Define rotation bivector.
  ϕ = @ga 3 α::Scalar * Π::Bivector

  # Define rotation generator.
  Ω = @ga 3 exp(-(ϕ::Bivector) / 2::Scalar)
  @test grade.(Ω) == (0, 2)
  @test all(Ω .≈ @ga 3 cos(Base.:*(0.5, α))::Scalar - Π::Bivector * sin(Base.:*(0.5, α))::Scalar)
  @test (@ga 3 Ω::(Scalar, Bivector) ⟑ inverse(Ω::(Scalar, Bivector))) == KVector{0,3}(1.0)

  x′ = @ga 3 begin
    Ω::(Scalar, Bivector)
    Ω ⟑ x::Vector ⟑ inverse(Ω)
  end
  @test x′ ≈ KVector{1,3}(0.0, sqrt(2), 0.0)

  # Do it more succinctly.
  @test_skip begin
    x′′ = @ga 3 begin
      # Define unit plane for the rotation.
      Π = a::Vector ⟑ b::Vector
      # Define rotation generator.
      Ω = exp((-α::Scalar / 2::Scalar) ⟑ Π)
      # Apply the rotation by sandwhiching x with Ω.
      Ω ⟑ x::Vector ⟑ reverse(Ω)
    end
    x′′ === x′
  end
end;
