struct KVector{K,T,D,N}
  grade::Val{K}
  elements::NTuple{N,T}
  KVector{K,T,D}(elements) where {K,T,D} = new{K,T,D,binomial(D,K)}(Val(K), elements)
end

const Bivector{T,D,N} = KVector{2,T,D,N}
const Trivector{T,D,N} = KVector{3,T,D,N}
const Quadvector{T,D,N} = KVector{4,T,D,N}
