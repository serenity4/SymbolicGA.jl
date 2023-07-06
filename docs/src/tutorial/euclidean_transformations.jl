#=

# Euclidean transformations

Geometric algebra being well suited for the representation of orthogonal transformations, we will go through different ways one can apply [Euclidean transformations](https://en.wikipedia.org/wiki/Rigid_transformation), using various geometric spaces of interest. Euclidean transformations are those which preserve the Euclidean distance between two points. They include reflections, rotations and translations expressed in an Euclidean space, as well as any composition of these. Orthogonal transformations only include reflections and rotations, but translations may be represented as orthogonal transformations within special non-Euclidean geometric spaces (quick hint: a translation may be viewed as a rotation following a circle of infinite radius).

## Transformations around the origin

The simplest transformations are defined around the origin. In contrast, take for example rotations around an axis that does not contain the origin, whose expression requires translations. Note that the translational part there is a trick to get back to the origin, apply the rotation, and revert the translation.

We will take $\mathbb{R}^3$ as our space of interest, and define a geometric algebra over it:

=#

using SymbolicGA

macro r3(args...) esc(:(SymbolicGA.@ga "+++" $(args...))) end

#=

Note that we are not defining $\mathbb{R}^3$ itself, but rather $\mathcal{G}(\mathbb{R}^3)$, the geometric space around $\mathbb{R}^3$.

This macro will allow us to use geometric algebra expressions, that will be processed and return Julia code that can be evaluated which implements the operations we aim to perform.

We will not attempt to express a rotation as an action around an axis vector. For example, in 2D space, we will have to construct a rotation without reference to a third axis, which would only exist in 3D space. We will construct our rotation by specifying the plane in which the action should be carried out.

Such a plane may be parametrized by two non-colinear vectors, say `a` and `b`. We form such an object using the outer product, which has the effect of joining vectors `a` and `b` into a geometric entity which contains them both:

=#

a = (1.0, 0.0, 0.0)
b = (0.0, 1.0, 0.0)
Π = @r3 a::1 ∧ b::1

# However, to avoid having to worry about whether `a` and `b` are unit vectors, we will unitize ("normalize", if you prefer) the plane `Π` such that it is a unit plane:

Π = @r3 unitize(a::1 ∧ b::1)

# Next, we will compute an object Ω which describes a rotation along such a plane. Once done, we will simply have to apply this object to any vector in 3D space to perform the rotation. Let's say we want a rotation of `π / 6`:

α = π / 6
Ω = @r3 exp(-(α::0 / 2::0) ⟑ Π::2)

# Let us also define the vector we want to rotate:
x = Tuple(2.0 .+ 1.5 .* randn(3))

# Now, how should we apply `Ω` to `x`? The operation we seek to carry out is an orthogonal transformation, using `Ω`, an object of a specific type: a versor.
# An orthogonal transformation is expressed via a versor product, which is provided as a specific operator `<<` defined in terms of the geometric product as `Ω ⟑ x ⟑ inv(Ω)`:

x′ = @r3 x::1 << Ω::(0, 2)
@assert x′ ≈ @r3 Ω::(0, 2) ⟑ x::1 ⟑ inv(Ω::(0, 2))
x′

# The inverse rotation may be applied using the inverse of our versor Ω:

x′′ = @r3 x′::1 << inv(Ω::(0, 2))

#=

We did get `x` back! But numbers being a bit hard to visualize, we prepared a small animation to see the rotation in action using Makie:

````@setup euclidean_transformations
using LinearAlgebra
@assert norm(x) ≈ norm(x′)
@assert x′′ ≈ KVector{1,3}(x)

include("../plots/rotation_origin.jl")
````

![](../plots/color_animation.mp4)
=#
