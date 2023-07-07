# Table of symbols

These tables list all the functions, operators and constants that are recognized as geometric operations, e.g. in code evaluated as part of a `@ga` block.

## Built-in functions

These functions are always accessible in any context, and cannot be overriden:
- `+(a, b, ...)`
- `-(a)`
- `-(a, b)`
- `reverse(a)`
- `antireverse(a)`
- `left_complement(a)`
- `right_complement(a)`
- `geometric_product(a, b, ...)`
- `exterior_product(a, b, ...)`
- `interior_product(a, b)`
- `commutator_product(a, b)`
- `inverse(a)`
- `exp(a)`

## Default symbols

Although any operation in geometric algebra may be performed using the built-in functions above, it is useful to have access to shorthands and other operators which build on these primitives. Therefore, default functions and operators are provided, as well as a few aliases and constants.

These are obtained with [`default_bindings`](@ref), and may be tweaked or removed if you choose to reimplement your own macro over `@ga` using [Expression generation](@ref) utilities.

### Functions

These functions define secondary operators that are more or less standard in geometric algebra. Many of these operators were extracted from [E. Lengyel's PGA poster](http://projectivegeometricalgebra.org/projgeomalg.pdf).

| Function | Definition |
|---|---|
`antidivision` | `inverse_dual(division(dual(a), dual(b)))`
`antiinverse` | `inverse_dual(inverse(dual(a)))`
`antiscalar_product` | `geometric_antiproduct(a, antireverse(a))::e̅`
`bulk_left_complement` | `geometric_product(antireverse(a), 𝟙)`
`bulk_norm` | `sqrt(interior_product(a, reverse(a)))::e`
`bulk_right_complement` | `geometric_product(reverse(a), 𝟙)`
`bulk` | `weight_left_complement(bulk_right_complement(a))`
`division` | `geometric_product(a, inverse(b))`
`exterior_antiproduct` | `inverse_dual(exterior_product(dual(a), dual(b)))`
`geometric_antiproduct` | `inverse_dual(geometric_product(dual(a), dual(b)))`
`geometric_norm` | `bulk_norm(a) + weight_norm(a)`
`interior_antiproduct` | `inverse_dual(interior_product(dual(a), dual(b)))`
`left_interior_antiproduct` | `exterior_product(a, right_complement(b))`
`left_interior_product` | `exterior_antiproduct(left_complement(a), b)`
`projected_geometric_norm` | `antidivision(bulk_norm(a), weight_norm(a))`
`right_interior_antiproduct` | `exterior_product(left_complement(a), b)`
`right_interior_product` | `exterior_antiproduct(a, right_complement(b))`
`scalar_product` | `geometric_product(a, reverse(a))::Scalar`
`unitize` | `antidivision(a, weight_norm(a))`
`versor_product` | `geometric_product(b, a, inverse(b))`
`weight_left_complement` | `geometric_antiproduct(𝟏, antireverse(a))`
`weight_norm` | `sqrt(interior_antiproduct(a, antireverse(a)))::e̅`
`weight_right_complement` | `geometric_antiproduct(𝟏, reverse(a))`
`weight` | `bulk_left_complement(weight_right_complement(a))`

### Operators

Convenient binary operators are defined by default to allow a more compact language to perform geometric operations. Note that there is no agreed-upon convention for these, and that is why we prefer to allow more advanced users to tweak them at will:

| Function | Unicode input | Expression |
|---|---|---|
| `a ⟑ b` | `\wedgedot` | `geometric_product(a, b)`
| `a ⟇ b` (Julia 1.10 or higher) | `\veedot` | `geometric_antiproduct(a, b)`
| `a ⩒ b` | `\veeodot` | `geometric_antiproduct(a, b)`
| `a ⋅ b` | `\cdot` | `interior_product(a, b)`
| `a ● b` | `\mdlgblkcircle` | `interior_product(a, b)`
| `a ○ b` | `\bigcirc` | `interior_antiproduct(a, b)`
| `a ⦿ b` | `\circledbullet` | `scalar_product(a, b)`
| `a ∧ b` | `\wedge` | `exterior_product(a, b)`
| `a ∨ b` | `\vee` | `exterior_antiproduct(a, b)`
| `a ⊣ b` | `\dashv` | `left_interior_product(a, b)`
| `a ⊢ b` | `\vdash` | `right_interior_product(a, b)`
| `a ⨼ b` | `\intprod` | `left_interior_antiproduct(a, b)`
| `a ⨽ b` | `\intprodr` | `right_interior_antiproduct(a, b)`
| `a << b` | | `versor_product(a, b)`
| `a / b` | | `division(a, b)`
| `a'` | | `reverse(a)`

!!! note
    In many materials about geometric algebra, the geometric product uses the same notation as the standard multiplication operator, `*` (or even juxtaposition). However, we prefer to use the `\wedgedot` symbol `⟑` [proposed by E. Lengyel](https://terathon.com/blog/projective-geometric-algebra-done-right/) to visually show its relationship with the inner and outer products, `⋅` and `∧`, and because it allows the use of an "anti-" operator to express the dual operator to the geometric product, the geometric antiproduct `⟇` (`\veedot`, available from Julia 1.10).
    There are also programming-related motivations, as `2x` destructures to `2 * x` which would rarely want to be considered as "the geometric product of 2 and `x`".


### Aliases

A few aliases are defined, with a short-hand for the inverse and a specific choice of a dual and dual inverse, along with common names for operators we named differently:

| Symbol | Alias |
|---|---|
| `inv` | `inverse`
| `dual` | `right_complement`
| `inverse_dual` | `left_complement`
| `regressive_product` | `exterior_antiproduct`
| `left_contraction` | `left_interior_product`
| `right_contraction` | `right_interior_product`

### Constants

These constants allow you to complactly express scalar and antiscalar units.

| Symbol | Input | Expression |
|---|---|---|
| `𝟏` | `\bfone` | `1::e`
| `𝟙` | `\bbone` | `1::e̅`

## Type annotations

Considering an $n$-dimensional space, the various ways to annotate a value are as follows:

| Annotation | Alias | Grade or basis blade
|---|---|---|
| `::e1` | | Basis vector $e_1$ (grade 1)
| `::e12` | `::e_1_2` | Basis blade $e_1 \wedge e_2$ (grade 2)
| `::e_11_12` | | Basis blade $e_{11} \wedge e_{12}$ (grade 2, more than one digit per index)
| `::Scalar` | `::0`, `::e` | $0$
| `::Vector` | `::1` | $1$
| `::Bivector` | `::2` | $2$
| `::Trivector` | `::3` | $3$
| `::Quadvector` | `::4` | $4$
| `::Antiscalar` | `::ē` | $n$
| `::(k, l, ...)` | | Tuple of multiple elements of grade $k$, $l$, ...
| `::(k + l + ...)` | | Concatenation of multiple elements of grade $k$, $l$, ...
| `::Multivector` | | Concatenation of all elements with grade $0$ to $n$
