abstract type Operation{S<:Signature} end

struct KVector{S<:Signature,K,T,D} <: Operation{S}
  data::D
end

struct Projection{S<:Signature,X<:Operation} <: Operation{S}
  x::X
end

struct OuterProduct{S<:Signature,X<:Operation,Y<:Operation} <: Operation{S}
  x::X
  y::Y
end

function Base.collect(op::KVector{S,K,T,D}) where {S,K,T,D}
  n = nelements(S, K)
  res = zero(D{N,T})
  for i in eachindex(op.data)
    res[i] = op.data[i]
  end
  res
end

nelements(::Type{S}, K::Int) where {S<:Signature} = binomial(dimension(S), K)
