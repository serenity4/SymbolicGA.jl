using Documenter

@testset "Doctests" begin
    DocMeta.setdocmeta!(SymbolicGA, :DocTestSetup, quote
        using SymbolicGA
    end)

    doctest(SymbolicGA)
end;
