using Documenter, SymbolicGA

analytics_asset = Documenter.Writers.HTMLWriter.HTMLAsset(
    :js,
    "https://plausible.io/js/script.js",
    false,
    Dict(:defer => "", Symbol("data-domain") => "serenity4.github.io"),
)

makedocs(;
    modules = [SymbolicGA],
    format = Documenter.HTML(
        prettyurls = true,
        assets = [analytics_asset],
        canonical = "https://serenity4.github.io/SymbolicGA.jl/stable/",
    ),
    pages = [
        "Home" => "index.md",
        "Reference" => [
            "API" => "reference/api.md",
        ],
    ],
    repo = "https://github.com/serenity4/SymbolicGA.jl/blob/{commit}{path}#L{line}",
    sitename = "SymbolicGA.jl",
    authors = "serenity4 <cedric.bel@hotmail.fr>",
    strict = true,
    doctest = false,
    checkdocs = :exports,
)

deploydocs(
    repo = "github.com/serenity4/SymbolicGA.jl.git",
)
