using Documenter

DocMeta.setdocmeta!(SymbolicGA, :DocTestSetup, quote
    using SymbolicGA
end)

doctest(SymbolicGA)
