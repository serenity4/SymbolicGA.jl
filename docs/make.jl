using Documenter, SymbolicGA

analytics_asset = Documenter.Writers.HTMLWriter.HTMLAsset(
    :js,
    "https://plausible.io/js/script.js",
    false,
    Dict(:defer => "", Symbol("data-domain") => "serenity4.github.io"),
)

makedocs(;
    modules = [SymbolicGA],
    format = Documenter.HTML(prettyurls = true, assets = [analytics_asset]),
    pages = [
        "Home" => "index.md",
        "API" => "api.md",
    ],
    repo = "https://github.com/serenity4/SymbolicGA.jl/blob/{commit}{path}#L{line}",
    sitename = "SymbolicGA.jl",
    authors = "serenity4 <cedric.bel@hotmail.fr>",
)

deploydocs(
    repo = "github.com/serenity4/SymbolicGA.jl.git",
)
