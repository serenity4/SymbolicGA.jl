macro cga3(args...)
  T, ex = length(args) == 1 ? (Tuple, args[1]) : args
  defs = quote
    n = 1.0::e4 + 1.0::e5
    n̄ = 1.0::e4 - 1.0::e5
    vector_3d(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
    magnitude2(x) = x ⦿ x
    norm(X) = -X ⋅ n
    # normalize(X) = X / norm(X)
    # radius2(X) = magnitude2(X)::Scalar / magnitude2(X ∧ n)::Scalar
    center(X) = X * n * X
  end
  if Meta.isexpr(ex, :block)
    pushfirst!(ex.args, defs.args...)
  else
    push!(defs.args, ex)
    ex = defs
  end

  esc(:(@ga $T (4, 1) $ex))
end

point(x) = @cga3 (0.5::Scalar * (2::Scalar * vector_3d(x) + magnitude2(vector_3d(x))::Scalar * n - n̄))::Vector
point_pair(A, B) = @cga3 A::Vector ∧ B::Vector
circle(A, B, C) = @cga3 point_pair(A, B)::Bivector ∧ C::Vector
line(A, B, C) = @cga3 point_pair(A, B)::Bivector ∧ n
sphere(A, B, C, D) = @cga3 circle(A, B, C)::Trivector ∧ D::Vector
plane(A, B, C) = @cga3 circle(A, B, C)::Trivector ∧ n
isnullvector(X) = iszero(@cga3 magnitude2(X::Vector))

@testset "3D Conformal Geometric Algebra" begin
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
  C1 = center(S1)
  @test_broken normalize(C1) == O
end;
