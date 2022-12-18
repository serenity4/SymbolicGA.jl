using Documenter, SymbolicGA

makedocs(;
    modules=[SymbolicGA],
    format=Documenter.HTML(prettyurls = true),
    pages=[
        "Home" => "index.md",
        "Introduction" => "intro.md",
        "API" => 
            "api.md"
        ,
    ],
    repo="https://github.com/serenity4/SymbolicGA.jl/blob/{commit}{path}#L{line}",
    sitename="SymbolicGA.jl",
    authors="serenity4 <cedric.bel@hotmail.fr>",
)

deploydocs(
    repo = "github.com/serenity4/SymbolicGA.jl.git",
)
