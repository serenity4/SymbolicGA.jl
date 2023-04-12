using SymbolicGA: metric, is_degenerate, triplet

@testset "Signature" begin
    @test Signature("+++") == Signature(3, 0)
    @test Signature("++-") == Signature(2, 1)
    @test Signature("+-𝟎") == Signature(1, 1, 1)

    @test triplet(Signature(1, 2, 3)) == (1, 2, 3)
    @test triplet(Signature(1, 2)) == (1, 2, 0)
    @test triplet(Signature(1)) == (1, 0, 0)

    @test !is_degenerate(Signature(4, 0, 0))
    @test !is_degenerate(Signature(1, 3, 0))
    @test is_degenerate(Signature(1, 1, 1))

    @test dimension(Signature(2, 1, 0)) == 3
    @test dimension(Signature(1, 1, 4)) == 6

    s = Signature(2, 1, 0)

    @test metric(s, Val(1), Val(1)) == 1
    @test metric(s, Val(1), Val(2)) == 0
    @test metric(s, Val(3), Val(3)) == -1
end;
