#=

# Creating user-defined geometric spaces

## Create a geometric space with default definitions

=#

using SymbolicGA

@geometric_space weird_space (3, 4, 5)

#-

x = rand(12); y = rand(12)
@weird_space x::1 ⟑ dual(y::1)

# ## Create a geometric space with extra definitions.

@geometric_space extra_space 3 quote
  >>(x, y) = versor_product(y, x)
  *(x, y) = geometric_product(y, x)
  I = 1.0::e123
end

#-

x = rand(3); y = rand(3)
@extra_space begin
  yᵈ = y::1 * I
  yᵈ >> x::1
end

# ## Create a geometric space with non-default bindings.

bindings = Bindings(refs = Dict(:* => :geometric_product))
@geometric_space no_defaults 3 bindings

#-

x = rand(3); y = rand(3)
@no_defaults x::1 * y::1

# Note that you will always have access to the [built-in functions](@ref builtins).
