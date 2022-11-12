@testset "Signature" begin
    @test Signature("+++") == Signature(3, 0)
    @test Signature("++-") == Signature(2, 1)
    @test Signature("+-𝟎") == Signature(1, 1, 1)

    @test !is_degenerate(Signature(4, 0, 0))
    @test !is_degenerate(Signature(1, 3, 0))
    @test is_degenerate(Signature(1, 1, 1))

    s = Signature(2, 1, 0)

    @test metric(s, Val(1), Val(1)) == 1
    @test metric(s, Val(1), Val(2)) == 0
    @test metric(s, Val(3), Val(3)) == -1
end;
