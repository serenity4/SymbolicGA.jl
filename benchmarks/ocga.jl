using SymbolicGA, BenchmarkTools

macro cga3(args...)
  definitions = quote
    n = 1.0::e4 + 1.0::e5
    n̄ = (-0.5)::e4 + 0.5::e5
    n̅ = n̄ # n\bar !== n\overbar but they display exactly the same.
    embed(x) = x[1]::e1 + x[2]::e2 + x[3]::e3
    magnitude2(x) = x ⦿ x
    point(x) = (embed(x) + (0.5::Scalar * magnitude2(embed(x))) * n + n̄)::Vector
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

function line_tests(A, B, P)
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
is_zero(x, y) = isapprox(x, y; atol = 1e-15)
is_positive(x::Number) = x ≥ -1e-15
is_zero_bivector(x) = is_zero(x, zero(KVector{2,Float64,5}))
is_on_line((t₁, t₂)) = is_zero_bivector(t₁[2]) && is_zero_bivector(t₂[2])
is_within_segment((t₁, t₂)) = is_positive(t₁[1][]) && is_positive(t₂[1][])

function is_on_segment(P, A, B)
  ret = line_tests(A, B, P)
  is_on_line(ret) && is_within_segment(ret)
end

@btime is_on_segment($(rand(3)), $(rand(3)), $(rand(3)))
