using LazyGeometricAlgebra: codegen_expression, postwalk, traverse

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

function generate(sig, types, ex)
  generated_ex = codegen_expression(sig, QuoteNode(:nested), nothing, annotate_variables(ex, types))
  ex = Expr(:block)
  for (k, v) in variables
    push!(ex.args, :($k = $v))
  end
  push!(ex.args, generated_ex)
  eval(ex)
end

types = Dict(:A => :Vector, :B => :Vector, :C => :Vector)
variables = Dict(:A => (1, 2, 3), :B => (10, 20, 30), :C => (100, 200, 300))

@testset "Operators" begin
  @testset "Associativity" begin
    @test generate(3, types, :(A + (B + C))) == generate(3, types, :((A + B) + C))
    @test generate(3, types, :(A * (B * C))) == generate(3, types, :((A * B) * C))
  end

  @testset "Distributivity" begin
    @test generate(3, types, :(A * (B + C))) == generate(3, types, :(A * B + A * C))
    @test generate(3, types, :(A ∧ (B + C))) == generate(3, types, :(A ∧ (B + C)))
  end

  @testset "Jacobi identity" begin
    lhs = @ga 4 :flatten Vector begin
      A = 1.3::e1 + 2.7::e12
      B = 0.7::e3 + 1.1::e123 + 3.05::e
      C = 0.7::e4 + 0.1::e2 + 1.59::e23 + 1.1::e124 + 3.05::e1234
      A × (B × C)
    end

    rhs = @ga 4 :flatten Vector begin
      A = 1.3::e1 + 2.7::e12
      B = 0.7::e3 + 1.1::e123 + 3.05::e
      C = 0.7::e4 + 0.1::e2 + 1.59::e23 + 1.1::e124 + 3.05::e1234
      -(B × (C × A) + C × (A × B))
    end

    @test all(lhs .≈ rhs)
  end

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

  #   @test simplified(sig, :∧, b, b̅) == antiscalar(sig)
  # end

  # @testset "De Morgan laws" begin
  #   @test right_complement(sig, exterior_product(x, y)) == exterior_antiproduct(sig, right_complement(sig, a), right_complement(sig, b))
  #   @test right_complement(sig, exterior_antiproduct(sig, x, y)) == exterior_product(right_complement(sig, a), right_complement(sig, b))
  #   @test left_complement(exterior_product(x, y)) == exterior_antiproduct(sig, left_complement(a), left_complement(b))
  #   @test left_complement(exterior_antiproduct(sig, x, y)) == exterior_product(left_complement(a), left_complement(b))
  # end
end;
