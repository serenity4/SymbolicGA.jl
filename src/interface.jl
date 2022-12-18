"""
    getcomponent(collection, i::Int)
    getcomponent(collection)

For geometric elements, retrieve the `i`th component of a geometric element backed by `collection`, using a linear index in the range `1:cumsum(nelements(signature, g) for g in grades)` where `signature` is the signature used for the algebra and `grades` the grades of the geometric element.

For example, a bivector in a geometric algebra with signature `(3, 0, 0)` has three components, and therefore will use indices from 1 to 3.
This falls back to `getindex(collection, i)`, therefore most collections won't need to extend this method.

Scalars and antiscalars are obtained with `getcomponent(scalar)`, which defaults to the identity function.
"""
function getcomponent end

getcomponent(x, i) = getindex(x, i)
getcomponent(x) = x

"""
    construct(T, components::Tuple)

Construct an instance of `T` from a tuple of components.

Defaults to `T(components)`.
"""
function construct end

construct(::Type{Tuple}, components) = components
construct(::Type{Vector{T}}, components) where {T} = collect(T, components)
construct(::Type{Vector}, components) = collect(components)
construct(::Type{T}, components) where {T} = T(components)
