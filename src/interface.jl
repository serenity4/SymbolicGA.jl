"""
    getcomponent(collection)
    getcomponent(collection, i)
    getcomponent(collection, j, i)

Retrieve a number from a collection to be interpreted as the component of a geometric entity.

`getcomponent(collection)` which defaults to `collection[]` is used to extract the only component of a scalar or antiscalar.

`getcomponent(collection, j, i)` which defaults to `collection[j][i]` is used to extract the i-th component the j-th geometric entity for a collection of multiple geometric vectors.

`getcomponent(collection, i)` which defaults to `collection[i]` is used to extract the i-th component of a single geometric entity or a set of geometric entities backed by `collection`. In the case of a set of geometric entities, this is semantically equivalent to `getcomponent(collection, j, i)` where a single `i` is computed from the cumulated sum of past indices, i.e. as if components from consecutive entities were concatenated together. See [`@ga`](@ref) for more information regarding which mode is used depending on the syntax for grade extraction.

Most collections will not need to extend this method, but is exposed should the need arise for a tigher control over how input data is accessed.
"""
function getcomponent end

getcomponent(x) = x[]
getcomponent(x, i) = x[i]
getcomponent(x, j, i) = x[j][i]

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
