function new_geometric_algebra(args...; signature, definitions = Expr(:block))
  T, ex = length(args) == 1 ? (Tuple, args[1]) : args
  if Meta.isexpr(ex, :block)
    pushfirst!(ex.args, definitions.args...)
  else
    push!(definitions.args, ex)
    ex = definitions
  end
  esc(:(@ga $T $(Expr(:tuple, signature...)) $ex))
end

macro cga3(args...)
  definitions = quote
    n = 1.0::e4 + 1.0::e5
    n̄ = 1.0::e4 - 1.0::e5
    vector_3d(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
    magnitude2(x) = x ⦿ x
    weight(X) = -X ⋅ n
    # normalize(X) = X / weight(X)
    # radius2(X) = magnitude2(X)::Scalar / magnitude2(X ∧ n)::Scalar
    center(X) = X * n * X
  end
  new_geometric_algebra(args...; signature = (4, 1, 0), definitions)
end

point(x) = @cga3 (0.5::Scalar * (2::Scalar * vector_3d(x) + magnitude2(vector_3d(x))::Scalar * n - n̄))::Vector
point_pair(A, B) = @cga3 A::Vector ∧ B::Vector
circle(A, B, C) = @cga3 point_pair(A, B)::Bivector ∧ C::Vector
line(A, B, C) = @cga3 point_pair(A, B)::Bivector ∧ n
sphere(A, B, C, D) = @cga3 circle(A, B, C)::Trivector ∧ D::Vector
plane(A, B, C) = @cga3 circle(A, B, C)::Trivector ∧ n

@testset "3D Conformal Geometric Algebra" begin
  isnullvector(X) = iszero(@cga3 magnitude2(X::Vector))
  @test (@cga3 n ⋅ n̄) == (@cga3 n̄ ⋅ n) == 2.0
  @test (@cga3 n ⦿ n) == (@cga3 n̄ ⦿ n̄) == 0
  A = point((1, 0, 0))
  B = point((0, 1, 0))
  C = point((0, 0, 1))
  D = point((-1, 0, 0))
  @test all(isnullvector, (A, B, C, D))
  S1 = sphere(A, B, C, D)
  @test S1 == (0., 2.0, 0., 0., 0.)
  O = point((0, 0, 0))
  C1 = @cga3 center(S1::Quadvector)
  @test_broken normalize(C1) == O
end;

macro pga3(args...)
  definitions = quote
    vector_3d(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
    magnitude2(x) = x ⦿ x
    point(x) = vector_3d(x) + 1.0::e4
  end
  new_geometric_algebra(args...; signature = (3, 0, 1), definitions)
end

@testset "3D Projective Geometric Algebra" begin
  A = point((1, 0, 0))
  B = point((0, 1, 0))
  C = point((0, 0, 1))
  D = point((-1, 0, 0))
end;
