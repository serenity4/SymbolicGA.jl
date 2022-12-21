# SymbolicGA

![tests](https://github.com/serenity4/SymbolicGA.jl/workflows/Run%20tests/badge.svg)
[![codecov](https://codecov.io/gh/serenity4/SymbolicGA.jl/branch/main/graph/badge.svg?token=5JSJGHYHCU)](https://codecov.io/gh/serenity4/SymbolicGA.jl)
[![docs-stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://serenity4.github.io/SymbolicGA.jl/stable)
[![docs-dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://serenity4.github.io/SymbolicGA.jl/dev)
[![ColPrac: Contributor's Guide on Collaborative Practices for Community Packages](https://img.shields.io/badge/ColPrac-Contributor's%20Guide-blueviolet)](https://github.com/SciML/ColPrac)
[![repo-status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Geometric Algebra (GA) library with compile-time transformations into linear algebra operations.

This package is ready for general use, but it still in active development and bugs may be frequently encountered along with incomplete or unsupported major features. You are welcome to report potential issues or to suggest improvements. When upgrading to a new major version, make sure to consult the [changelog](https://github.com/serenity4/SymbolicGA.jl/blob/main/CHANGELOG.md) to be aware of any major breakages.

## Basic usage

```julia
using SymbolicGA

# Compute the determinant of a 4x4 matrix.
# Let A₁, A₂, A₃ and A₄ be the matrix columns.
A₁, A₂, A₃, A₄ = ntuple(_ -> ntuple(_ -> rand(), 4), 4)
# The determinant is the four-dimensional "volume" of the subspace spanned by all four column vectors.
# This is trivially generalized to `n`-by-`n` matrices by using a signature of `n` and wedging all `n` column vectors.
Δ = @ga 4 A₁::Vector ∧ A₂::Vector ∧ A₃::Vector ∧ A₄::Vector
# We got an antiscalar out as a `KVector{4}`.
# Extract the component with `[]`.
Δ[]
```

For advanced usage, tutorials and references, please consult the [official documentation](https://github.com/serenity4.github.io/SymbolicGA.jl/dev).

## Performance

This library applies rules of geometric algebra at compile-time to generate performant code for runtime execution. The resulting instructions are scalar operations, which should be very fast and comparable to hand-written optimized numerical code:

```julia
using StaticArrays: @SVector, SMatrix
using LinearAlgebra: det
using BenchmarkTools: @btime
A₁ = @SVector rand(4)
A₂ = @SVector rand(4)
A₃ = @SVector rand(4)
A₄ = @SVector rand(4)
A = SMatrix([A₁ A₂ A₃ A₄])
Δ = @ga 4 A₁::Vector ∧ A₂::Vector ∧ A₃::Vector ∧ A₄::Vector
@assert Δ[] ≈ det(A)
@btime (@ga 4 $A₁::Vector ∧ $A₂::Vector ∧ $A₃::Vector ∧ $A₄::Vector)[]
@btime det($A)
```

```
5.743 ns (0 allocations: 0 bytes) # SymbolicGA
5.916 ns (0 allocations: 0 bytes) # LinearAlgebra
```
