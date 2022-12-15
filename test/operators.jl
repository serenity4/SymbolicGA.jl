using LazyGeometricAlgebra: codegen_expression, postwalk, traverse, blade_left_complement, blade_right_complement, dimension
using Combinatorics: combinations

all_blades(sig::Signature) = [blade(indices) for indices in combinations(1:dimension(sig))]

function ga_eval(sig_ex, ex; flatten = :nested, T = nothing, varinfo = nothing)
  eval(codegen_expression(sig_ex, QuoteNode(flatten), T, ex, varinfo))
end

@testset "Operators" begin
  sig = 3
  varinfo = VariableInfo(refs = Dict(:A => :((1, 2, 3)::Vector), :B => :((10, 2, 30)::Vector), :C => :((10, 200, 3)::Vector)))
  generate = ex -> ga_eval(sig, ex; varinfo)

  @testset "Associativity" begin
    @test generate(:(A + (B + C))) == generate(:((A + B) + C))
    @test generate(:(A * (B * C))) == generate(:((A * B) * C))
  end

  @testset "Distributivity" begin
    @test generate(:(A * (B + C))) == generate(:(A * B + A * C))
    @test generate(:(A ∧ (B + C))) == generate(:(A ∧ (B + C)))
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

  @testset "Complements" begin
    for sig in [Signature.(2:3); Signature(3, 0, 1); Signature(4, 1); Signature.(6:10)]
      @test all(all_blades(sig)) do b
        (b ∧ blade_right_complement(sig, b) == antiscalar(sig)) &
          (blade_left_complement(sig, b) ∧ b == antiscalar(sig))
      end
    end
  end

  @testset "Exterior (anti)product" begin
    @test (@ga 3 3.0::e ∧ 4.0::e)[] == 12.0
    @test (@ga 3 3.0::e̅ ∨ 4.0::e̅)[] == 12.0

    @testset "De Morgan laws" begin
      sig = (3, 0, 1)
      varinfo = VariableInfo(refs = Dict(
        :A => :((1, 2, 3, 4)::Vector),
        :B => :((10, 2, 30, 4)::Vector),
        :C => :((10, 200, 30, 400)::Vector),
        :A̅ => :(right_complement(A)),
        :B̅ => :(right_complement(B)),
        :A̲ => :(left_complement(A)),
        :B̲ => :(left_complement(B)),
      ))
      generate = ex -> ga_eval(sig, ex; varinfo)

      @test generate(:(right_complement(A ∧ B))) == generate(:(A̅ ∨ B̅))
      @test generate(:(right_complement(A ∨ B))) == generate(:(A̅ ∧ B̅))
      @test generate(:(left_complement(A ∧ B))) == generate(:(A̲ ∨ B̲))
      @test generate(:(left_complement(A ∨ B))) == generate(:(A̲ ∧ B̲))
    end
  end
end;
