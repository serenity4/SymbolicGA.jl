"""
getcomponent(collection, i::Int)

Retrieve the `i`th component of a geometric element backed by `collection`, using a linear index in the range `1:cumsum(nelements(signature, g) for g in grades)`
where `signature` is the signature used for the algebra and `grades` the grades of the geometric element.
For example, a bivector in the geometric algebra over ℝ³ has three components, and therefore will use component indices from 1 to 3.

This falls back to `getindex(collection, i)`, so for most collections you won't need to extend this method.
"""
function getcomponent end

getcomponent(x, i) = getindex(x, i)
