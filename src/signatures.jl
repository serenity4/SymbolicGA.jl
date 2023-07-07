"""
    Signature(positive::Int, negative::Int = 0, degenerate::Int = 0)
    Signature(str::AbstractString) # Signature("++-𝟎")

Signature of an Euclidean or pseudo-Euclidean space.

This signature encodes a space with a metric such that the first `P` basis vectors square to 1, the following `N` to -1 and the following `D` to 0.
The metric evaluates to zero between two distinct basis vectors.
"""
struct Signature{P,N,D} end

Base.broadcastable(x::Signature) = Ref(x)

Signature(positive::Int, negative::Int = 0, degenerate::Int = 0) = Signature{positive, negative, degenerate}()
Signature(string::AbstractString) = Signature(count.(["+", "-", "𝟎"], Ref(string))...)

positive(::Signature{P}) where {P} = P

negative(::Signature{P,N}) where {P,N} = N

degenerate(::Signature{P,N,D}) where {P,N,D} = D

dimension(::Signature{P,N,D}) where {P,N,D} = P + N + D

is_degenerate(sig::Signature) = degenerate(sig) ≠ 0

triplet(sig::Signature) = (positive(sig), negative(sig), degenerate(sig))

metric(::Signature{P,N,D}, ::Val{I}) where {P,N,D,I} = I <= P ? 1 : I <= P + N ? -1 : 0
metric(::Signature{P,N,D}, i::Integer) where {P,N,D} = i <= P ? 1 : i <= P + N ? -1 : 0
metric(sig::Signature{P,N,D}, i::Val{I}, j::Val{I}) where {P,N,D,I} = metric(sig, i)
metric(::Signature, ::Val{I}, ::Val{J}) where {I,J} = 0

Base.show(io::IO, sig::Signature) = print(io, Signature, "(\"", sig == Signature(0, 0, 0) ? "Ø" : "<" * join(["+", "-", "𝟎"] .^ triplet(sig)) * ">", "\")")

nelements(s::Signature, k::Int) = binomial(dimension(s), k)
