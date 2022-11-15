"""
    getcomponent(collection, i::Int)
    getcomponent(scalar)

For geometric elements except scalars and pseudoscalars, retrieve the `i`th component of a geometric element backed by `collection`, using a linear index in the range `1:cumsum(nelements(signature, g) for g in grades)` where `signature` is the signature used for the algebra and `grades` the grades of the geometric element.
For example, a bivector in the geometric algebra over ℝ³ has three components, and therefore will use component indices from 1 to 3.
This falls back to `getindex(collection, i)`, so for most collections you won't need to extend this method.

Scalars and pseudoscalars are obtained with `getcomponent(scalar)` and default to the identity function.
"""
function getcomponent end

getcomponent(x, i) = getindex(x, i)
getcomponent(x) = x

"""
    construct(T, components)

Construct an instance of `T` from a tuple of components.
"""
function construct end

construct(::Type{Tuple}, components) = components
construct(::Type{Vector{T}}, components) where {T} = collect(T, components)
construct(::Type{Vector}, components) = collect(components)
construct(::Type{T}, components) where {T} = T(components)
