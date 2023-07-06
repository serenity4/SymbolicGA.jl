using SymbolicGA: extract_weights, input_expression, extract_expression, restructure, expand_variables, argument_count, fill_argument_slots

@testset "Macro frontend" begin
  @testset "Function definition" begin
    f = :($(@arg(1)) + $(@arg(2)))
    @test argument_count(f) == 2

    @test_throws "Not enough function arguments" fill_argument_slots(f, [:x], :f)
    @test fill_argument_slots(f, [:x, :y], :f) == :(x + y)
  end

  @testset "Function and variable expansion" begin
    sig = Signature(3)

    # Recursive reference.
    ex = quote
      x = x::Vector
      x ⟑ x
    end
    @test expand_variables(ex, Bindings()) == :(x::Vector ⟑ x::Vector)

    # Interleaved references/function calls.
    ex = quote
      b1 = 1::e1
      g(z) = z + b1
      y = (1, 2, 3)
      x = g(y::Vector)
      x
    end
    ex2 = expand_variables(ex, Bindings())
    @test ex2 == :((1, 2, 3)::Vector + 1::e1)

    bindings = Bindings(refs = Dict(
      :x => 2.4,
      :z => :(x::e),
    ))
    ex = :(z ⦿ z)
    ex2 = expand_variables(ex, bindings)
    @test ex2 == :(2.4::e ⦿ 2.4::e)

    bindings = Bindings(refs = Dict(
      :A => :((1, 2, 3)::Vector),
      :B => :((10, 2, 30)::Vector),
      :C => :((10, 200, 30)::Vector),
      :A̅ => :(right_complement(A)),
      :B̅ => :(right_complement(B)),
      :A̲ => :(left_complement(A)),
      :B̲ => :(left_complement(B)),
    ))
    ex = :(A̅ ∧ B̅)
    ex2 = expand_variables(ex, merge!(builtin_bindings(), bindings))
    @test ex2 == :(right_complement((1, 2, 3)::Vector) ∧ right_complement((10, 2, 30)::Vector))

    sig = Signature(4, 1, 0)
    ex = quote
      n = 1.0::e4 + 1.0::e5
      magnitude2(x) = x ⦿ x
      weight(X) = (-X ⋅ n)::Scalar
      normalize(X) = X / weight(X)
      radius2(X) = (magnitude2(X) / magnitude2(X ∧ n))::Scalar
      radius(X) = normalize(radius2(X))::Scalar
      radius(S::Quadvector)
    end
    ex2 = expand_variables(ex, builtin_bindings(; warn_override = false))
    symbols = expression_nodes(ex -> in(ex, (:radius, :radius2, :normalize, :weight, :magnitude2, :n)), ex2, Expr)
    @test isempty(symbols)

    @testset "Redefinition warnings" begin
      bindings = Bindings(funcs = Dict(
        :geometric_antiproduct => :(0::e),
      ))
      @test_logs (:warn, r"Redefinition of built-in function") merge!(builtin_bindings(), bindings)

      bindings = Bindings(refs = Dict(
        :𝟏 => :(1::e̅),
      ))
      @test_logs (:warn, r"Redefinition of built-in variable") merge!(builtin_bindings(), bindings)

      bindings = Bindings(funcs = Dict(
        :geometric_antiproduct => :(0::e),
      ); warn_override = false)
      @test_logs merge!(builtin_bindings(), bindings)

      ex = quote
        f(x) = x
        f(x, y) = x + y
        f(1::e, 2::e1)
      end
      @test_logs (:warn, r"user-defined function") expand_variables(ex, Bindings())

      ex = quote
        x = 3
        x = 4
        x::Scalar
      end
      @test_logs (:warn, r"user-defined variable") expand_variables(ex, Bindings())
    end
  end

  sig = Signature(1, 1, 1)
  cache = ExpressionCache(sig)
  ws = extract_weights(cache, :x, 1; offset = 0)
  @test length(ws) == 3
  @test all(isexpr(w, COMPONENT) && length(w) == 2 && dereference(cache, w[2]) == i for (i, w) in enumerate(ws))
  ex = input_expression(cache, :x, 2)
  @test isexpr(ex, ADDITION, 3)
  @test ex[1] == factor(cache, first(ws)) * blade(cache, 1, 2)

  ex = extract_expression(:((x::Vector ⟑ y::Bivector)::Trivector), sig, builtin_bindings())
  ex2 = restructure(ex)
  @test isexpr(ex2, KVECTOR, 1)
  @test isweighted(ex2[1]) && isexpr(ex2[1][2], BLADE)
  @test isa(string(ex2), String)

  ex = @macroexpand @ga (2, 1) Tuple x::Vector ∧ y::Vector + x::Vector ⟑ z::Antiscalar
  @test isa(ex, Expr)
  x = (1, 2, 3)
  y = (4, 5, 6)
  z = 3

  # Yields 1 bivector.
  res = @ga (2, 1) Tuple x::Vector ∧ y::Vector + x::Vector ⟑ z::Antiscalar
  @test isa(res, NTuple{3,Int})

  x = (1, 2)
  y = (0, 50)
  res = @ga 2 Tuple x::Vector ∧ y::Vector + x[1]::Scalar ⟑ z::Antiscalar
  @test res == (1 * 50 + 1 * 3,)
  # Yields 1 vector and 1 antiscalar.
  res = @ga 2 x::Vector ∧ y::Vector + x::Vector ⟑ z::Antiscalar
  @test isa(res, Tuple{<:KVector{1}, <:KVector{2}})
  res2 = @ga 2 Vector x::Vector ∧ y::Vector + x::Vector ⟑ z::Antiscalar
  @test collect.(res) == res2 && all(isa(x, Vector{Int}) for x in res2)
  res3 = @ga 2 Vector{Float64} x::Vector ∧ y::Vector + x::Vector ⟑ z::Antiscalar
  @test res2 == res3 && all(isa(x, Vector{Float64}) for x in res3)

  x = (1.0, 2.0, 3.0)
  y = (50.0, 70.0, 70.0)
  # Yields 1 scalar and 1 bivector.
  res = @ga 3 x::1 ⟑ y::1
  @test grade.(res) == (0, 2)
  @test res[1][] == sum(x .* y)

  res = @ga 3 begin
    x::Vector
    x ⟑ x
  end
  @test grade(res) == 0

  res2 = @ga 3 begin
    x = (1.0, 2.0, 3.0)::Vector
    x ⟑ x
  end
  @test res === res2

  # The `1::e12` gets simplified to `e12`.
  res = @ga 3 :flattened (1::e1 ⟑ 1::e1 + 1::e12)::Multivector
  @test res == (1, 1, 0, 0)

  # Preserve element types.
  res = @ga 3 :flattened (1::e1 ⟑ 1::e1 + 1.0::e12)::Multivector
  @test res == (1, 1.0, 0, 0)

  res = @ga 3 :flattened (1::e1 ⟑ 1::e1 + 2::e12)::Multivector
  @test res == (1, 2, 0, 0)

  res = @ga 3 ((x::Vector)')
  @test res == KVector{1,3}(x)
  res = @ga 3 ((x::Bivector)')
  @test res == KVector{2,3}((-).(x))

  x = (1, 2, 3)
  res = (@ga 3 (x::Vector ⟑ x::Bivector ∧ x::Vector + 2::e12)::Multivector)
  @test grade(res) == 2

  @test (@ga 3 right_complement(1::e2)) == (@ga 3 1::e31)

  y = (101, 102, 103)
  @test (@ga 3 Tuple (x::1 × y::1)::2) == (@ga 3 Tuple (x::1 ∧ y::1))

  z = (x..., y...)
  z_nested = (x, y)
  @test (@ga 3 𝟏 ∧ z::(1 + 2)) == (@ga 3 𝟏 ∧ z_nested::(1, 2)) == (@ga 3 𝟏 ∧ (x::1 + y::2))
  @test (@ga 2 (1, 2, 3)::(0 + 1)) == (@ga 2 ((1,), (2, 3))::(0, 1)) == (KVector{0,2}(1), KVector{1,2}(2, 3))

  z2 = (3, z..., 2)
  @test (@ga 3 𝟏 ∧ z2::Multivector) == (@ga 3 𝟏 ∧ (3::e + x::1 + y::2 + 2::e̅))

  @test_throws "Unknown grade projection" @eval @ga 3 x::Unknown

  # Ability to interpolate parts of expressions to shield them from processing.
  @test (@ga 3 $(x[1] * x[2])::e1) == KVector{1,3}(x[1] * x[2], 0, 0)
end;
