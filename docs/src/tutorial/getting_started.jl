#=

# Getting started

SymbolicGA is a library build upon the metaprogramming capabilities of the language to express and simplify geometric algebra expressions at compile-time. This brings the potential of optimality to its implementation, but may feel a bit odd to use compared to most Julia libraries. Additionally, there were unforeseen advantages to keeping geometric algebra operators as pure notation in a symbolic realm: it allows naming things as you like without worrying about choosing names for Julia functions or structures that satisfy everyone (because, unfortunately, consensus on notation for geometric algebra is pretty far away at the moment).

This package defines two high-level symbols:
- A single macro, [`@ga`](@ref), which allows you to setup your own geometric algebra context and evaluate arbitrary geometric algebra expressions.
- A single structure, [`KVector`](@ref), which simply wraps components with additional information regarding what type of geometric entity it is, and the dimension of the space inside which it was produced.

The [`KVector`](@ref) structure is generally useful to understand what you are getting out of [`@ga`](@ref) and is produced by default. However, the idea of this package is to be able to directly use and generate data from and into your own data structures, provided that a certain interface is fulfilled. For example, if you are designing a computational geometry library, you can define your own types such as `Point`, `Line`, `Circle` etc and use them in `@ga`. No need to juggle with types from another library! After all, geometric algebra defines semantically meaningful transformations, but cares little about how the data has been abstracted over or how it is stored in memory.

A few more advanced features will allow you to seamlessly integrate geometric algebra within your own codebase:
- Utilities for code generation, including [`codegen_expression`](@ref), [`Bindings`](@ref), [`default_bindings`](@ref) will be useful to build macros that automate
- Interface functions [`SymbolicGA.getcomponent`](@ref) and [`SymbolicGA.construct`](@ref) to use your own types within generated expressions, in case the defaults based on indexing and constructors do not fit with the design of your data structures.

We will explain how these features work separately and together, to unlock the expression of geometric algebra within Julia in a performant and non-intrusive way.

## Using [`@ga`](@ref)

The simplest way to define your own geometric space and carry operations on it is to use the [`@ga`](@ref) macro. For example, to construct the geometric space over $\mathbb{R}^2$, noted $\mathcal{G}(\mathbb{R}^2)$, you first specify a signature for this space. Then, you can perform any operations you like, provided that you annotate the type of your values:

=#

using SymbolicGA

x = 1.0
y = 2.0
@ga 2 x::e1 + y::e2

# We simply constructed a simple vector within $\mathcal{G}(\mathbb{R}^2)$ by associating scalar components with basis vectors `e1` and `e2`. Most of the time though, unless you desire to construct a geometric entity with sparse components, you will prefer providing vectors as annotated iterables, such as

@ga 2 (x, y)::Vector

# Note that this `Vector` annotation *does not mean `Base.Vector`*; it means a mathematical vector in the geometric space of interest. Now, let us take the geometric product ⟑ (`\wedgedot`) of two vectors:

a = (x, y)
b = rand(2)

@ga 2 a::Vector ⟑ b::Vector

#=

Here, we obtained a mathematical object composed of both a 0-vector (scalar) and a 2-vector (bivector) part. In $\mathcal{G}(\mathbb{R}^2)$, a bivector has a single component, but with most other spaces bivectors have more; for any embedded space of dimension `n`, the number of elements for an entity with grade `k` is `binomial(n, k)`.

!!! note
    If you dislike the use of non-ASCII characters for major operators, you can use standard function names instead of operators, such as `geometric_product(x, y)` (equivalent to `x ⟑ y`) (see [Table of symbols](@ref)).

What if we wanted only the scalar part or the bivector part? We can project the result into either grade 0 or grade 2, respectively:

=#

@ga 2 (a::Vector ⟑ b::Vector)::Scalar

#-

@ga 2 (a::Vector ⟑ b::Vector)::Bivector

# Since it may be a bit tedious to type in these names by hand, when all we really need is to express the grade in these annotations, we can directly use a number on the right-hand side of `::` (see [Type annotations](@ref)).

@ga 2 (a::1 ⟑ b::1)::0

# In this particular case, getting the lowest and highest grade component of a geometric product is what defines the inner and outer products, `⋅` (`\cdot`) and `∧` (`\wedge`). See [Table of symbols](@ref) for a complete list of symbols and operators.

@ga 2 a::1 ⋅ b::1

#-

@ga 2 a::1 ∧ b::1
