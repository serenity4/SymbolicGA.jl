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
b = (-2.0, 1.0, 0.0)
Π = @r3 a::1 ∧ b::1

# You will have noticed that neither `a` or `b` are unit vectors, and `Π` is not a unit plane. `a` and `b` are not orthogonal either. That is not a problem; we will handle that later, for now all we need is the plane in which they are contained, and therefore any choice of non-colinear vectors `a` and `b` will work.

# Next, we compute an object `Ω` which describes a rotation along a plane `Π` with angle `α`; `Π` needs to be a unit plane, so we add a unitization ("renormalization") step to enforce that.

α = π / 15
Ω = @r3 exp(-α::0 / 2::0 ⟑ unitize(Π::2))

# `Ω` is a particular kind of object: a versor. Versors are geometric products of invertible vectors, and the exponentiation of a bivector is one way to obtain such a versor. Seeing that we obtain a scalar and a bivector, you could wonder: why not define `Ω = @r3 a::1 ⟑ b::1`? This is because `Ω` would then describe a very specific rotation: a rotation in the plane formed by `a` and `b` - so far so good -, but of twice the angle between `a` and `b` multiplied by `norm(a) * norm(b)`. If we want to apply a rotation with an arbitrary angle, we essentially have to find and join two unit vectors in the plane of rotation such that they form half the desired angle of rotation between them, `α / 2`. This is what the exponential form above parametrizes: for any `α`, the resulting `Ω` is the versor corresponding to two such vectors (in the plane of rotation, with an angle of `α / 2`), describing a rotation in that plane by an angle `α`.

# Say we want to rotate
x = (3.0, 4.0, 5.0)

# How should we apply `Ω` to `x`? The operation we seek to carry out is an orthogonal transformation, using `Ω`. In geometric algebra, orthogonal transformations are obtained by a specific operation on versors, termed the *versor product* (informally named the sandwich product). The versor product on `x` by `Ω` is defined as `Ω ⟑ x ⟑ inverse(Ω)`, which is defined by default with the `x << Ω` operator.

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

Non-unit vectors `a` and `b` are represented in green and cyan. The bivector formed by `a` and `b` is represented as a purple semi-transparent parallelogram, with its dual - the normal of the plane - represented as a solid purple line.

## Transformations around arbitrary axes

We just performed a rotation around the origin. How can we get rotations around arbitrary points in space?

Although planes arised naturally to describe 3D rotations around the origin, things change a bit when rotating around arbitrary points. It is still valid to define rotations within specific planes, *but that plane must contain the object being rotated*. As we were working with geometries defined around the origin, it was always the case. In fact, the specificity of standard vector spaces such as $\mathcal{G}(\mathbb{R}^2)$ or $\mathcal{G}(\mathbb{R}^3)$ or their "vanilla" geometric spaces such as $\mathcal{G}(\mathbb{R}^2)$ or $\mathcal{G}(\mathbb{R}^3)$ is that geometric entities always contain the origin. A point is always represented by a vector pointing from the origin to a location, and this prevents vectors from being represented as entities invariant by affine transformations (which include translations). If you translate a vector in $\mathbb{R}^3$, only the target location of the vector is translated, but the origin remains the same. For geometric spaces, the situation is the same - they build from the base vector space, and therefore inherit their limitations. For example, bivectors are formed of vectors pointing from the origin, and therefore represent planes parametrized by three points: the point of origin, and the two vector target locations.

This limitation in terms of representation does not prevent translations from being defined, but they do not integrate well with the mathematical model; if you translate the whole space, geometric operations do not represent the same thing before and after the translation. Just like vectors, a translated bivector represents a different plane, and not one that is a translation of the original. The "mathematical" way of saying all this, is that Euclidean space (and Euclidean geometry - geometries represented using Euclidean space) is not invariant with respect to translation.

Coming from another angle, you could ask: "Why, in the first place, do we special-case an origin - aren't all points in 3D space considered the same?". Well, an Euclidean space is one in which angles are defined and points are parametrized by numbers - coordinates -, enabling various measurements; it is the space as seen from a specific location, the origin. No wonder that translating it results in a different space.

We therefore need another space. The spaces that are invariant with respect to translations (and rotations) are called affine spaces. In these spaces, points and vectors are different from each other; adding two points is undefined, while vectors can be added and points translated by vectors. Affine spaces are the intrinsic space we generally compute in - but because we need to represent points with numbers, we identify an origin to allow for a coordinate system, and carry operations in Euclidean space. Forgetting about the origin and staying purely in affine space is tricky; even if we can mathematically describe elements in affine spaces, without an origin we cannot describe them with numerical values.

How can we get out of this situation? We need to find another space, which has an origin so we can numerically represent objects with coordinates, but which also contains an affine space as a subspace. The classical setting is to use a four-dimensional Euclidean space, and use a three-dimensional hyperplane located at `w = 1`; in other terms, Euclidean 3D space is embedded within the four-dimensional Euclidean space with the map $(x, y, z) \rightarrow (x, y, z, 1)$. It works well in removing the 3D origin as a special point - it happens to be `(0, 0, 0, 1)`, but is treated just like any other point on that hyperplane from the persepctive of a four-dimensional space. We will go slightly further however, and trade a tiny bit of intuition by defining a geometric space with signature $(3, 0, 1)$ instead of $(4, 0, 0)$ to obtain more elegant and simpler formulas that describe translations and rotations. A walkthrough of the geometric spaces over $\mathbb{R}^3$ (2D projective space) and $\mathbb{R}^4$ (3D projective space) is provided in the books *Geometric Algebra for Computer Science - An Object-Oriented Approach to Geometry* and *Aspects of Geometric Algebra in Euclidean, Projective and Conformal Space*.

=#
