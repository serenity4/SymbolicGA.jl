using LazyGeometricAlgebra: generate_expression, postwalk, traverse, codegen

function annotate_variables(ex, types::Dict{Symbol,<:Any})
  already_annotated = Set{Symbol}()
  traverse(ex) do ex
    Meta.isexpr(ex, :(::)) && isa(ex.args[1], Symbol) && push!(already_annotated, ex.args[1])
    nothing
  end
  postwalk(ex) do ex
    isa(ex, Symbol) || return ex
    !in(ex, already_annotated) && haskey(types, ex) && return :($ex::$(types[ex]))
    ex
  end
end

generate_typed_expression(sig, types, ex) = generate_expression(sig, annotate_variables(ex, types))

types = Dict(:A => :Vector, :B => :Vector, :C => :Vector)
A = (1, 2, 3)
B = (10, 20, 30)
C = (100, 200, 300)

@testset "Operators" begin
  @testset "Associativity" begin
    ex1 = generate_typed_expression(3, types, :(A + (B + C)))
    ex2 = generate_typed_expression(3, types, :((A + B) + C))
    @test ex1 == ex2
    jex1 = codegen(Vector, ex1)
    jex2 = codegen(Vector, ex2)
    @test jex1 == jex2
    @test eval(jex1) == eval(jex2)

    ex1 = generate_typed_expression(3, types, :(A * (B * C)))
    ex2 = generate_typed_expression(3, types, :((A * B) * C))
    @test_broken ex1 == ex2
    jex1 = codegen(Vector, ex1)
    jex2 = codegen(Vector, ex2)
    @test_broken jex1 == jex2
    @test eval(jex1) == eval(jex2)
  end

  @testset "Distributivity" begin
    ex1 = generate_typed_expression(3, types, :(A * (B + C)))
    ex2 = generate_typed_expression(3, types, :(A * B + A * C))
    @test_broken ex1 == ex2
    jex1 = codegen(Vector, ex1)
    jex2 = codegen(Vector, ex2)
    @test_broken jex1 == jex2
    @test eval(jex1) == eval(jex2)

    ex1 = generate_typed_expression(3, types, :(A ∧ (B + C)))
    ex2 = generate_typed_expression(3, types, :(A ∧ B + A ∧ C))
    @test_broken ex1 == ex2
    jex1 = codegen(Vector, ex1)
    jex2 = codegen(Vector, ex2)
    @test_broken jex1 == jex2
    @test eval(jex1) == eval(jex2)
  end

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

  # @testset "Complements" begin
  #   sig = Signature(3, 0, 1)

  #   b = blade(1, 3)
  #   b̅ = right_complement(sig, b)
  #   @test b̅ == blade(4, 2)
  #   b̅̅ = right_complement(sig, b̅)
  #   @test b̅̅ == b

  #   b = blade(4)
  #   b̅ = right_complement(sig, b)
  #   @test b̅ == blade(3, 2, 1)
  #   b̅̅ = right_complement(sig, b̅)
  #   @test b̅̅ == -b

  #   @test simplified(sig, :∧, b, b̅) == pseudoscalar(sig)
  # end

  # @testset "De Morgan laws" begin
  #   @test right_complement(sig, exterior_product(x, y)) == exterior_antiproduct(sig, right_complement(sig, a), right_complement(sig, b))
  #   @test right_complement(sig, exterior_antiproduct(sig, x, y)) == exterior_product(right_complement(sig, a), right_complement(sig, b))
  #   @test left_complement(exterior_product(x, y)) == exterior_antiproduct(sig, left_complement(a), left_complement(b))
  #   @test left_complement(exterior_antiproduct(sig, x, y)) == exterior_product(left_complement(a), left_complement(b))
  # end
end
