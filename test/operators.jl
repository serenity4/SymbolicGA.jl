using SymbolicGA: postwalk, traverse, blade_left_complement, blade_right_complement, dimension
using Combinatorics: combinations

all_blades(cache::ExpressionCache) = [blade(cache, indices) for indices in combinations(1:dimension(cache.sig))]

function ga_eval(sig_ex, ex; flattening = :nested, T = nothing, varinfo = nothing)
  eval(codegen_expression(sig_ex, ex; flattening, T, varinfo))
end

@testset "Operators" begin
  sig = 3
  varinfo = VariableInfo(refs = Dict(:A => :((1, 2, 3)::Vector), :B => :((10, 2, 30)::Vector), :C => :((10, 200, 3)::Vector)))
  generate = ex -> ga_eval(sig, ex; varinfo)

  @testset "Associativity" begin
    @test generate(:(A + (B + C))) == generate(:((A + B) + C))
    @test generate(:(A ⟑ (B ⟑ C))) == generate(:((A ⟑ B) ⟑ C))
  end

  @testset "Distributivity" begin
    @test generate(:(A ⟑ (B + C))) == generate(:(A ⟑ B + A ⟑ C))
    @test generate(:(A ∧ (B + C))) == generate(:(A ∧ (B + C)))
  end

  @testset "Jacobi identity" begin
    lhs = @ga 4 begin
      A = 1.3::e1 + 2.7::e12
      B = 0.7::e3 + 1.1::e123 + 3.05::e
      C = 0.7::e4 + 0.1::e2 + 1.59::e23 + 1.1::e124 + 3.05::e1234
      A × (B × C)
    end

    rhs = @ga 4 begin
      A = 1.3::e1 + 2.7::e12
      B = 0.7::e3 + 1.1::e123 + 3.05::e
      C = 0.7::e4 + 0.1::e2 + 1.59::e23 + 1.1::e124 + 3.05::e1234
      -(B × (C × A) + C × (A × B))
    end

    @test all(lhs .≈ rhs)
  end

  @testset "Complements" begin
    for cache in ExpressionCache.([Signature.(2:3); Signature(3, 0, 1); Signature(4, 1); Signature.(6:10)])
      @test all(all_blades(cache)) do b
        (b ∧ blade_right_complement(b) == antiscalar(cache)) &
          (blade_left_complement(b) ∧ b == antiscalar(cache))
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

  @testset "Interior (anti)product" begin
    @test (@ga 3 ●(3.0::e, 1.0::e1))[] == 0.
    @test (@ga 3 ●(3.0::e̅, (2.0::e̅)'))[] == 6.0
    @test (@ga 3 ●(3.0::e12, 2.0::e2)) == KVector{1,3}(6., 0., 0.)
    @test (@ga 3 ●((sqrt(2)::e1 + sqrt(2)::e2), (sqrt(2)::e1 + sqrt(2)::e2)'))[] ≈ 4.0

    @test (@ga 3 ○(3.0::e̅, 1.0::e23))[] == 0.
    @test (@ga 3 ○(3.0::e, (2.0::e)'))[] == -6.0
    @test (@ga 3 ○(3.0::e12, 2.0::e2)) == KVector{2,3}(0., 0., 6.)
    @test (@ga 3 ○((sqrt(2)::e23 + sqrt(2)::e13), antireverse(sqrt(2)::e23 + sqrt(2)::e13)))[] ≈ 4.0
  end

  @testset "Bulk and weight" begin
    sig = (3, 0, 1)
    varinfo = VariableInfo(refs = Dict(
      :x => 2.4,
      :y => 3.2,
      :z => :(x::e + y::e̅),
      :p1 => :(1.2::e1 + 1.56::e2 + 1.65::e3 + 1.0::e4),
      :p2 => :((-1.2)::e1 - 1.0::e2 + 0.0::e3 - 1.0::e4),
    ))
    generate = ex -> ga_eval(sig, ex; varinfo)

    @test generate(:(left_complement(z))) == generate(:(bulk_left_complement(z) + weight_left_complement(z)))
    @test generate(:(right_complement(z))) == generate(:(bulk_right_complement(z) + weight_right_complement(z)))

    @test generate(:(bulk(z))) == generate(:(x::e))
    @test generate(:(weight(z))) == generate(:(y::e̅))
    @test generate(:(weight(p1))) == generate(:(1.0::e4))
    @test generate(:(weight(p2))) == generate(:((-1.0)::e4))
  end

  @testset "Norms & unitization" begin
    sig = (3, 0, 1)
    varinfo = VariableInfo(refs = Dict(
      :p => :(8.4::e1 + 4.2::e4),
      :punit => :(2.0::e1 + 1.0::e4),
    ))
    generate = ex -> ga_eval(sig, ex; varinfo)

    @test generate(:(bulk_norm(1::e1))) == generate(:(1.0::e))
    # TODO: Make sure generate(:(0.0::e)) results in a KVector whose element type is Float64.
    # @test generate(:(bulk_norm(1::e4))) == generate(:(0.0::e))
    @test generate(:(bulk_norm(1::e4))) == KVector{0,4}(0.0)

    @test generate(:(weight_norm(1::e4))) == generate(:(1.0::e̅))
    @test generate(:(weight_norm(1::e1))) == KVector{4,4}(0.0)
    @test generate(:(weight_norm(3.2::e1 + 4.0::e4))) == generate(:(4.0::e̅))

    @test generate(:(unitize(p))) == generate(:punit)
  end
end;
