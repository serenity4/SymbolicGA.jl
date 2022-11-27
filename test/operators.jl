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
