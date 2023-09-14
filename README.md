# SymbolicGA

![tests](https://github.com/serenity4/SymbolicGA.jl/workflows/Run%20tests/badge.svg)
[![codecov](https://codecov.io/gh/serenity4/SymbolicGA.jl/branch/main/graph/badge.svg?token=5JSJGHYHCU)](https://codecov.io/gh/serenity4/SymbolicGA.jl)
[![docs-stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://serenity4.github.io/SymbolicGA.jl/stable)
[![docs-dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://serenity4.github.io/SymbolicGA.jl/dev)
[![ColPrac: Contributor's Guide on Collaborative Practices for Community Packages](https://img.shields.io/badge/ColPrac-Contributor's%20Guide-blueviolet)](https://github.com/SciML/ColPrac)
[![repo-status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Geometric Algebra (GA) library based on just-in-time symbolic processing to turn algebraic expressions into optimized code.

This package is ready for general use, but it still in active development and bugs may be frequently encountered along with incomplete or unsupported major features. You are welcome to report potential issues or to suggest improvements. When upgrading to a new major version, make sure to consult the [changelog](https://github.com/serenity4/SymbolicGA.jl/blob/main/CHANGELOG.md) to be aware of any major breakages.

### JuliaCon talk

An introduction to geometric algebra with quick presentation of this library is available on YouTube.

- [Slides]([Slides](https://docs.google.com/presentation/d/e/2PACX-1vQ9trJBYfvZXCEoArxRQwYhS_tzGBYOfeY-s7aGZhE8_J-VPbztXbPPgW9uNTjUUrNbf9JWIYjLLngW/pub?start=false&loop=false&delayms=3000))

[![Geometric Algebra at compile-time with SymbolicGA.jl](assets/talk.png)](https://youtu.be/lD4tNcHVjX4)

## Basic usage

```julia
using SymbolicGA

# Compute the determinant of a 4x4 matrix.
# Let A₁, A₂, A₃ and A₄ be the matrix columns.
A₁, A₂, A₃, A₄ = ntuple(_ -> rand(4), 4)
# The determinant is the four-dimensional "volume" of the subspace spanned by all four column vectors.
# This is trivially generalized to `n`-by-`n` matrices by using a signature of `n` and wedging all `n` column vectors.
Δ = @ga 4 A₁::1 ∧ A₂::1 ∧ A₃::1 ∧ A₄::1
# We got an antiscalar out as a `KVector{4}`.
# Extract the component with `[]`.
Δ[]

# Let's compute the rotation of a vector x by α radians in the plane formed by a and b.
# We do this in 3D, but this works in any dimension with the appropriate signature; 2D but also 4D, 5D, etc.
a = (1.0, 0.0, 0.0)
b = (0.0, 1.0, 0.0)
x = (1.0, 1.0, 0.0)
α = π / 6
# Define a unit plane for the rotation.
# The unitization ensures we don't need `a` and `b` to be orthogonal nor to be unit vectors.
# If these conditions are otherwise met, unitization can be skipped.
Π = @ga 3 unitize(a::1 ∧ b::1)

# Define rotation generator.
Ω = @ga 3 exp(-(0.5α)::0 ⟑ Π::2)
# Apply the rotation with the versor product of x by Ω.
x′ = @ga 3 x::1 << Ω::(0, 2)
@assert collect(x′) ≈ [0.36602540378443876, 1.3660254037844386, 0.0]
```

For advanced usage, tutorials and references, please consult the [official documentation](https://serenity4.github.io/SymbolicGA.jl/stable/).

## Performance

This library applies rules of geometric algebra during macro expansion to generate numerically performant code. The resulting instructions are scalar operations, which should be fast and comparable to hand-written type-stable numerical code.

Here is an example benchmark to compute determinants, compared with LinearAlgebra:

```julia
using StaticArrays: @SVector, SMatrix
using LinearAlgebra: det
using BenchmarkTools: @btime
mydet(A₁, A₂, A₃, A₄) = @ga(4, A₁::1 ∧ A₂::1 ∧ A₃::1 ∧ A₄::1)[]
A₁ = @SVector rand(4)
A₂ = @SVector rand(4)
A₃ = @SVector rand(4)
A₄ = @SVector rand(4)
A = SMatrix([A₁ A₂ A₃ A₄])
@assert mydet(A₁, A₂, A₃, A₄) ≈ det(A)
@btime det($A)
@btime mydet($A₁, $A₂, $A₃, $A₄)
```

```julia
4.845 ns (0 allocations: 0 bytes) # LinearAlgebra
13.485 ns (0 allocations: 0 bytes) # SymbolicGA
```

This snippet performs a 3D vector rotation along an arbitrary plane and angle. Note that part of the timing is only about building the rotation operator Ω, which would correspond to building a rotation matrix in conventional approaches.

```julia
function rotate_3d(x, a, b, α)
  Π = @ga 3 unitize(a::1 ∧ b::1)
  Ω = @ga 3 exp(-(0.5α)::0 ⟑ Π::2)
  rotate_3d(x, Ω)
end

rotate_3d(x, Ω) = @ga 3 x::1 << Ω::(0, 2)

a = (1.0, 0.0, 0.0)
b = (2.0, 2.0, 0.0) # some arbitrary non-unit vector non-orthogonal to `a`.
x = (2.0, 0.0, 0.0)
α = π / 6

x′ = rotate_3d(x, a, b, α)
@assert x′ ≈ KVector{1,3}(2cos(π/6), 2sin(π/6), 0.0)
@btime rotate_3d($a, $b, $x, $α)
Ω = @ga 3 exp(-(0.5α)::0 ⟑ (a::1 ∧ b::1))
@btime rotate_3d($x, $Ω)
```

```julia
# 3D rotation, including both the construction of the rotation operator from a plane and its application.
40.582 ns (0 allocations: 0 bytes)
# Application of rotation operator.
8.310 ns (0 allocations: 0 bytes)
```

It should be noted that in theory any performance gap can be addressed, as we have total control over what code is emitted.
