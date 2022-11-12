using Documenter, LazyGeometricAlgebra

makedocs(;
    modules=[LazyGeometricAlgebra],
    format=Documenter.HTML(prettyurls = true),
    pages=[
        "Home" => "index.md",
        "Introduction" => "intro.md",
        "API" => 
            "api.md"
        ,
    ],
    repo="https://github.com/serenity4/LazyGeometricAlgebra.jl/blob/{commit}{path}#L{line}",
    sitename="LazyGeometricAlgebra.jl",
    authors="serenity4 <cedric.bel@hotmail.fr>",
)

deploydocs(
    repo = "github.com/serenity4/LazyGeometricAlgebra.jl.git",
)
