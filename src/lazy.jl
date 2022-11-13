abstract type Operation{S<:Signature} end

struct Basis{I,S<:Signature} <: Operation{S}
  Basis{I}(s::S) where {I,S<:Signature} = new{I,S}()
end

struct Scalar{S<:Signature,T} <: Operation{S}
  val::T
  Scalar(s::S, val::T) where {S<:Signature,T} = new{S,T}(val)
end

struct KVector{K,S<:Signature,D} <: Operation{S}
  data::D
  function KVector{K}(s::S, data::D) where {S<:Signature,K,D}
    @assert nelements(S, K) == length(data)
    new{K,S,D}(data)
  end
end

nelements(::Type{S}, K::Int) where {S<:Signature} = binomial(dimension(S), K)
container_type(op::KVector{K,S,D}) where {S,K,D} = D

struct GeometricProduct{S<:Signature,X<:Operation,Y<:Operation} <: Operation{S}
  x::X
  y::Y
  GeometricProduct(x::X, y::Y) where {S,X<:Operation{S},Y<:Operation{S}} = new{S,X,Y}(x, y)
end

Base.:*(x::Operation, y::Operation) = GeometricProduct(x, y)

struct Projection{S<:Signature,K,X<:Operation} <: Operation{S}
  x::X
end

container_type(op::Projection) = container_type(op.x)

function apply(res, op::Projection{S,K1,KVector{K2,S}}) where {S,K1,K2}
  K1 !== K2 && return zero(container_type(op))
  apply(res, op.x)
end

struct Dual{S<:Signature,X<:Operation} <: Operation{S}
  x::X
end

struct Reverse{S<:Signature,X<:Operation} <: Operation{S}
  x::X
end

struct Addition{S<:Signature,T<:NTuple{<:Any,<:Operation}} <: Operation{S}
  xs::T
end

# `r` and `s` are the grades of homogeneous multivectors (r- and s-vectors).
grade(::Type{<:Projection{S,K}}) where {S,K} = K
grade(::Type{<:KVector{K,S}}) where {S,K} = K

grades(::Type{Dual{S}}, r) where {S} = (i for i in 0:dimension(S) if i ≠ r)

function inner_product(s, x, y)
  r, s = grade(x), grade(y)
  projection(s, geometric_product(s, x, y), iszero(r) || iszero(s) ? nothing : abs(r - s))
end

function outer_product(s, x, y)
  r, s = grade(x), grade(y)
  projection(s, geometric_product(s, x, y), r + s > dimension(s) ? nothing : r + s)
end
