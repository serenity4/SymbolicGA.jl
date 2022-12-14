"""
Geometric `K`-vector with eltype `T` with `N` elements in a geometric algebra of dimension `D`.
"""
struct KVector{K,T,D,N}
  elements::NTuple{N,T}
  function KVector{K,D}(elements::NTuple{N,T}) where {K,T,D,N}
    (0 ≤ K ≤ D) || error("Cannot construct $K-vector in a geometric algebra of dimension $D")
    N === binomial(D, K) || error("Expected ", binomial(D, K), " elements, got $N")
    new{K,T,D,N}(elements)
  end
  KVector{K,D}(elements::Tuple) where {K,D} = KVector{K,D}(promote(elements...))
end

const Bivector{T,D,N} = KVector{2,T,D,N}
const Trivector{T,D,N} = KVector{3,T,D,N}
const Quadvector{T,D,N} = KVector{4,T,D,N}

@forward KVector.elements (Base.iterate,)

Base.getindex(kvec::KVector) = only(kvec.elements)
Base.getindex(kvec::KVector, i::Integer) = kvec.elements[i]

Base.eltype(::Type{<:KVector{<:Any,T}}) where {T} = T
Base.length(::Type{<:KVector{<:Any,<:Any,<:Any,N}}) where {N} = N

grade(::Type{<:KVector{K}}) where {K} = K

for f in (:(Base.eltype), :(Base.length), :grade)
  @eval $f(kvec::KVector) = $f(typeof(kvec))
end

Base.convert(::Type{NTuple{N,T}}, kvec::KVector{<:Any,T,<:Any,N}) where {T,N} = kvec.elements
