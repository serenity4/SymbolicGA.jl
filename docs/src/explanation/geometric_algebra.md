# Geometric Algebra

This section is dedicated to briefly introducing geometric algebra, and provide resources for further information. Note that geometric algebra is a vast topic, drawing from specialized and advanced branches of geometry in mathematics.

## Introduction

Geometric algebra is an algebraic framework which allows elegant and efficient expressions of geometric entities and transformations. For example, its projective version over $\mathbb{R}^3$ compactly expresses intersections between points, lines and planes without introducing coordinates nor equations; the conformal version additionally expresses circles and spheres, and unifies translations and rotations in a single geometric object, again in a coordinate-free manner. For those familiar with quaternions, these are contained in the geometric algebra over the vector space $\mathbb{R}^3$, and can be better understood intuitively as part of a bigger framework than usually presented. Rotations can furthermore be expressed in the geometric algebra over the vector space $\mathbb{R}^2$ without having to resort to embedding it within $\mathbb{R}^3$, as is the case for a treatment of rotations using the standard cross product. This specific algebra over $\mathbb{R}^2$ contains complex numbers as a subalgebra, in the same way that the geometric algebra over $\mathbb{R}^3$ contains quaternions as a subalgebra.

The major advantages of such a framework resides in:
- Requiring a low symbolic complexity, with clean abstractions that do not require special casings on coordinate systems nor looking up complex formulas to express common geometric operations.
- Providing good intuition about the nature of geometric operations, with clear semantics assigned to many of the operators within geometric algebra and connections to numerous branches of mathematics related to geometry.

The implications to programming are largely tied to the simplicity and consistency over many applications. In certain contexts, it is possible to perform more efficient operations than standard methods by exploiting the sparsity of certain structures; for example, 3D rotations can be expressed by matrices, but with intrinsically fewer degrees of freedom than a general 3x3 matrix. However, the most noticeable improvements reside in lower code complexity, resulting in easier maintenance and understandability of implementations of geometric operations.

Nevertheless, it should be noted that geometric algebra requires a certain mastery before showing its usefulness. In a way, this is a complicated swiss knife which has a steep learning curve and which departs from classic approaches to geometry that one may be more familiar with. Learning about geometric algebra is not an easy task, and is notably recommended for implementers and users of algorithms related to computational geometry and for those that are curious and desiring to gain a deeper insight about the different angles from which geometry can be viewed.

In particular, as an algebraic framework and a highly convenient utility, it very rarely provides new results unknown to other branches of mathematics; but it can easily unlock them to those unused to advanced abstract reasoning, and foster new developments through a unified language and insights that result from its elegance.

## Resources

#### Introductions
- [Siggraph 2019 talk](https://www.youtube.com/watch?v=tX4H_ctggYo) (video)
- [Cambridge course](http://geometry.mrao.cam.ac.uk/2016/10/geometric-algebra-2016/)
- Introductory book: *Vince, J. (2008). Geometric algebra for computer graphics. Springer Science & Business Media.*

#### Geometric Calculus
- Reference book: *Hestenes, D., & Sobczyk, G. (2012). Clifford algebra to geometric calculus: a unified language for mathematics and physics (Vol. 5). Springer Science & Business Media.*
- [Advanced tutorial](https://www.youtube.com/watch?v=ItGlUbFBFfc) (video)
- Compact summary (advanced): *Macdonald, A. (2017). A survey of geometric algebra and geometric calculus. Advances in Applied Clifford Algebras, 27(1), 853-891.*

Online resources:
- [bivector.net](https://bivector.net)
- Projective geometric algebra:
  - [projectivegeometricalgebra.org](http://projectivegeometricalgebra.org/)
  - [Wiki](https://rigidgeometricalgebra.org/wiki/index.php?title=Main_Page)
  - [3D PGA poster](http://projectivegeometricalgebra.org/projgeomalg.pdf) (E. Lengyel)
