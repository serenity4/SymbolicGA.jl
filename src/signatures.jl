struct Signature{P,N,D} end

Base.broadcastable(x::Signature) = Ref(x)

Signature(positive, negative=0, degenerate=0) = Signature{positive, negative, degenerate}()
Signature(string::AbstractString) = Signature(count.(["+", "-", "𝟎"], Ref(string))...)

positive(::Signature{P}) where {P} = P

negative(::Signature{P,N}) where {P,N} = N

degenerate(::Signature{P,N,D}) where {P,N,D} = D

dimension(::Type{Signature{P,N,D}}) where {P,N,D} = P + N + D

is_degenerate(sig::Signature) = degenerate(sig) ≠ 0

triplet(sig::Signature) = (positive(sig), negative(sig), degenerate(sig))

metric(::Signature{P,N,D}, ::Val{I}) where {P,N,D,I} = I <= P ? 1 : I <= P + N ? -1 : 0
metric(::Signature{P,N,D}, i::Integer) where {P,N,D} = i <= P ? 1 : i <= P + N ? -1 : 0
metric(sig::Signature{P,N,D}, i::Val{I}, j::Val{I}) where {P,N,D,I} = metric(sig, i)
metric(::Signature, ::Val{I}, ::Val{J}) where {I,J} = 0

Base.show(io::IO, sig::Signature) = print(io, sig == Signature(0, 0, 0) ? "Ø" : "<" * join(["+", "-", "𝟎"] .^ triplet(sig)) * ">")

nelements(::Type{S}, K::Int) where {S<:Signature} = binomial(dimension(S), K)
