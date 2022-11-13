using LazyGeometricAlgebra, Test
using LazyGeometricAlgebra: KVector, Basis, Scalar

s = Signature(3, 0, 0)

p = KVector{1}(s, (1, 2, 3))
q = KVector{1}(s, (1, 2, 3))
p * q

e1 = Basis{(1,)}(s) * Scalar(s, 1.0)
e2 = Basis{(2,)}(s) * Scalar(s, 1.0)
e3 = Basis{(3,)}(s) * Scalar(s, 1.0)

e1 * e2 * e3 * e3
