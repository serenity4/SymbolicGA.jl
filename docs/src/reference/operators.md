# Table of symbols

## Built-in functions

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

## Operators

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


## Aliases

| Symbol | Alias |
|---|---|
| `inv` | `inverse`
| `dual` | `right_complement`
| `inverse_dual` | `left_complement`

## Constants

| Symbol | Input | Expression |
|---|---|---|
| `𝟏` | `\bfone` | `1::e`
| `𝟙` | `\bbone` | `1::e̅`

## Type annotations

Considering an $n$-dimensional space:

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
