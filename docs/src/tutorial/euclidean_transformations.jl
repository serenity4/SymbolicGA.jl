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

a = (2.0, 0.0, 0.0)
b = (1.1, 1.5, 0.0)
Π = @r3 a::1 ∧ b::1

# You will have noticed that neither `a` or `b` are unit vectors, and `Π` is not a unit plane. `a` and `b` are not orthogonal either. That is not a problem; all we need is the plane in which they are contained, and therefore any choice of non-colinear vectors `a` and `b` will work!

# Next, we compute an object `Ω` which describes a rotation along such a plane:

α = π / 15
Ω = @r3 exp((α::0 / 2::e) ⟑ Π::2)

# `Ω` is a particular kind of object: a versor. Versors are geometric products of invertible vectors, and the exponentiation of a bivector is one way to obtain such a versor. Seeing that we obtain a scalar and a bivector, you could wonder: why not define `Ω = @r3 a::1 ⟑ b::1`? This is because `Ω` would then describe a very specific rotation: a rotation in the plane formed by `a` and `b` - so far so good -, but of twice the angle between `a` and `b`. If we want to apply a rotation with an arbitrary angle, we essentially have to find and join two vectors in the plane of rotation such that they form half the desired angle of rotation between them, `α / 2`. This is what the exponential form above parametrizes: for any `α`, the resulting `Ω` is the versor corresponding to two such vectors (in the plane of rotation, with an angle of `α / 2`), allowing a rotation in that plane by an angle `α`.

# Say we want to rotate
x = (3.0, 4.0, 5.0)

# How should we apply `Ω` to `x`? The operation we seek to carry out is an orthogonal transformation, using `Ω`. In geometric algebra, orthogonal transformations are obtained by a specific operation on versors, termed the *versor product* (also named the sandwich product more informally). The versor product on `x` by `Ω` is defined as `Ω ⟑ x ⟑ inverse(Ω)`, which is defined by default with the `x << Ω` operator.

x′ = @r3 x::1 << Ω::(0, 2)
@assert x′ ≈ @r3 Ω::(0, 2) ⟑ x::1 ⟑ inverse(Ω::(0, 2))

using LinearAlgebra: norm # hide
@assert norm(x) ≈ norm(x′) # hide

x′

# The inverse rotation may be applied using the inverse of our versor Ω:

@assert KVector{1,3}(x) ≈ @r3 x′::1 << inv(Ω::(0, 2)) # hide
x′′ = @r3 x′::1 << inv(Ω::(0, 2))

#=

We did get `x` back! But numbers being a bit hard to visualize, we prepared a small animation to see the rotation in action using Makie:

````@setup euclidean_transformations
include("../plots/rotation_origin.jl")
````

![](../plots/color_animation.mp4)
=#

#=

Alright, so that is a simple rotation around the origin.

=#
