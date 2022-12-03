function new_geometric_algebra(args...; signature, definitions = Expr(:block))
  T, ex = length(args) == 1 ? (Tuple, args[1]) : args
  if Meta.isexpr(ex, :block)
    pushfirst!(ex.args, definitions.args...)
  else
    push!(definitions.args, ex)
    ex = definitions
  end
  esc(:(@ga $T $signature $ex))
end

macro cga3(args...)
  definitions = quote
    n = 1.0::e4 + 1.0::e5
    n̄ = (-0.5)::e4 + 0.5::e5
    vector_3d(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
    magnitude2(x) = x ⦿ x
    weight(X) = (-X ⋅ n)
    normalize(X) = X / weight(X)
    radius2(X) = magnitude2(X / weight(X))::Scalar
    radius(X) = normalize(radius2(X))::Scalar
    center(X) = X * n * X
    # For spheres `S` defined as vectors, and points `X` defined as vectors as well.
    distance(S, X) = (S ⋅ X) / (weight(S) * weight(X))
  end
  new_geometric_algebra(args...; signature = (4, 1, 0), definitions)
end

point(x) = @cga3 (vector_3d(x) + (0.5::Scalar * magnitude2(vector_3d(x))) * n + n̄)::Vector
point_pair(A, B) = @cga3 A::Vector ∧ B::Vector
circle(A, B, C) = @cga3 point_pair(A, B)::Bivector ∧ C::Vector
line(A, B, C) = @cga3 point_pair(A, B)::Bivector ∧ n
sphere(A, B, C, D) = @cga3 circle(A, B, C)::Trivector ∧ D::Vector
plane(A, B, C) = @cga3 circle(A, B, C)::Trivector ∧ n

@testset "3D Conformal Geometric Algebra" begin
  isnullvector(X) = iszero(@cga3 magnitude2(X::Vector))
  @test (@cga3 n ⋅ n̄) == (@cga3 n̄ ⋅ n) == -1.0
  @test (@cga3 magnitude2(n ⦿ n)) == (@cga3 magnitude2(n̄ ⦿ n̄)) == 0
  A = point((1, 0, 0))
  B = point((0, 1, 0))
  C = point((0, 0, 1))
  D = point((-1, 0, 0))
  @test all(isnullvector, (A, B, C, D))
  S1 = sphere(A, B, C, D)
  @test S1 == (0., 2.0, 0., 0., 0.)
  @test (@cga3 normalize((A .* 2)::Vector)) == A
  O = point((0, 0, 0))
  C1 = @cga3 center(S1::Quadvector)::Vector
  @test (@cga3 normalize(C1::Vector)) == O
  (@cga3 weight(S1::Quadvector))
  @test_broken (@cga3 radius(S1::Quadvector)) == 1
end;

macro pga3(args...)
  definitions = quote
    vector_3d(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
    magnitude2(x) = x ⦿ x
    point(x) = vector_3d(x) + 1.0::e4
    regressive_product(x, y) = dual(dual(x) ∧ dual(y))
  end
  new_geometric_algebra(args...; signature = (3, 0, 1), definitions)
end

struct Camera
  optical_center::NTuple{4,Float64} # ::Vector
  image_plane::NTuple{4,Float64} # ::Trivector
end
Camera(A₁, A₂, A₃, A₄) = Camera(@pga3(point(A₄)), @pga3 point(A₁) ∧ point(A₂) ∧ point(A₃))

project_point(camera::Camera, x) = @pga3 begin
  line = camera.optical_center::Vector ∧ point(x)
  regressive_product(line, camera.image_plane::Trivector)::(1 + 2 + 3)
end

@testset "3D Projective Geometric Algebra" begin
  A = @pga3 point((1, 0, 0))
  B = @pga3 point((0, 1, 0))
  C = @pga3 point((1, 1, 0))
  D = @pga3 point((0, 0, 1))

  image_plane = @pga3 A::Vector ∧ B::Vector ∧ C::Vector
  optical_center = D

  camera = Camera(optical_center, image_plane)
  @test_broken project_point(camera, (1.2, 1, 0)) == (1.2, 1, 0)
end;
