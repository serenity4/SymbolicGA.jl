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
A₁, A₂, A₃, A₄ = ntuple(_ -> rand(4), 4)
# The determinant is the four-dimensional "volume" of the subspace spanned by all four column vectors.
# This is trivially generalized to `n`-by-`n` matrices by using a signature of `n` and wedging all `n` column vectors.
Δ = @ga 4 A₁::Vector ∧ A₂::Vector ∧ A₃::Vector ∧ A₄::Vector
# We got an antiscalar out as a `KVector{4}`.
# Extract the component with `[]`.
Δ[]

# Let's compute the rotation of a vector x by α radians in the plane formed by a and b.
# We do this in 3D, but this works in any dimension with the appropriate signature; 2D but also 4D, 5D, etc.
a = (1.0, 0.0, 0.0)
b = (0.0, 1.0, 0.0)
x = (1.0, 1.0, 0.0)
α = π / 6
# Define unit plane for the rotation.
Π = @ga 3 a::Vector ⟑ b::Vector
# Define rotation generator.
Ω = @ga 3 exp((-α::Scalar / 2::Scalar) ⟑ Π::(Scalar, Bivector))
# Apply the rotation with the versor product of x by Ω.
x′ = @ga 3 x::Vector << Ω::(Scalar, Bivector)
```

For advanced usage, tutorials and references, please consult the [official documentation](https://github.com/serenity4.github.io/SymbolicGA.jl/dev).

## Performance

This library applies rules of geometric algebra at compile-time to generate performant code for runtime execution. The resulting instructions are scalar operations, which should be fast and comparable to hand-written optimized numerical code:

```julia
using StaticArrays: @SVector, SMatrix
using LinearAlgebra: det
using BenchmarkTools: @btime
mydet(A₁, A₂, A₃, A₄) = @ga(4, A₁::Vector ∧ A₂::Vector ∧ A₃::Vector ∧ A₄::Vector)[]
A₁ = @SVector rand(4)
A₂ = @SVector rand(4)
A₃ = @SVector rand(4)
A₄ = @SVector rand(4)
A = SMatrix([A₁ A₂ A₃ A₄])
@assert mydet(A₁, A₂, A₃, A₄) ≈ det(A)
@btime det($A)
@btime mydet($A₁, $A₂, $A₃, $A₄)
```

```
4.845 ns (0 allocations: 0 bytes) # LinearAlgebra
13.485 ns (0 allocations: 0 bytes) # SymbolicGA
```

It should be noted that in theory any performance gap can be addressed, as we have total control over what code is emitted.
