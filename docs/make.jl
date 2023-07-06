using Documenter, SymbolicGA, Literate

function julia_files(dir)
    files = reduce(vcat, [joinpath(root, file) for (root, dirs, files) in walkdir(dir) for file in files])
    sort(filter(endswith(".jl"), files))
end

function replace_edit(content)
    haskey(ENV, "JULIA_GITHUB_ACTIONS_CI") && return content
    # Linking does not work locally, but we can make
    # the warning go away with a hard link to the repo.
    replace(
        content,
        r"EditURL = \".*<unknown>/(.*)\"" => s"EditURL = \"https://github.com/JuliaGPU/Vulkan.jl/tree/master/\1\"",
    )
end

function generate_markdowns()
    dir = joinpath(@__DIR__, "src")
    Threads.@threads for file in julia_files(dir)
        Literate.markdown(
            file,
            dirname(file);
            postprocess = replace_edit,
            documenter = true,
        )
    end
end

generate_markdowns()

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
        "Explanation" => [
            "Geometric Algebra" => "explanation/geometric_algebra.md",
        ],
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
