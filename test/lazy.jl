function example_1()
  p = Vec3(1, 0, 0)
  q = Vec4(0.2, 0.2, 0.6, -0.4)

  # Setup metric and mapping from data to geometric elements.
  algebra = signature(3, 0, 0)
  p = vector(algebra, p)
  q = bivector(algebra, q.xyz) + scalar(algebra, q.w)

  # Construct a lazy sandwich operation `p << q` and evaluate it right away.
  # Equivalent to `collect(q * p * ~q)`.
  p′ = collect(p << q)
  # If needed: p′ = collect(vector(p << q))

  # p′ is a Vec3 where all its components are computed from sums and products composed of p[1], p[2], p[3], q.xyz[1], q.xyz[2], q.xyz[3] and q.w.
  # The Vec3 type is derived as the corresponding Vector for this particular algebra, given information about Vec, promoted element types and
  # the length of a vector in this algebra.

  p′
end

function example_2()
  p = Vec3(1, 0, 0)
  q = Vec4(0.2, 0.2, 0.6, -0.4)

  p′ = collect(@ga (3, 0, 0) p::Vector << (q.xyz::Bivector + q.w::Scalar))
  # The signature does not need to be a literal, e.g.
  # p′ = collect(@ga signature p::Vector << (q.xyz::Bivector + q.w::Scalar))

  p′
end
