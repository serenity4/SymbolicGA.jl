#=

## [Use your own types in geometric algebra expressions](@id howto-integration)

### Inputs

If the type you use supports indexing, e.g. `Vector`, it already works:

=#

using SymbolicGA

x = rand(3)
y = rand(3)

@ga 3 x::1 ∧ y::1

# If your type does not support indexing, and you don't want it to, overload `SymbolicGA.getcomponent(::T, [i::Int, [j::Int]])`:

struct MyInputType{T}
  values::Vector{T}
end

SymbolicGA.getcomponent(x::MyInputType, i::Int) = x.values[i]

x = MyInputType(rand(3))
y = MyInputType(rand(3))

@ga 3 x::1 ∧ y::1

#=

For scalars and aggregates of objects with multiple grades, you will need to overload `SymbolicGA.getcomponent(::T)` and `SymbolicGA.getcomponent(::T, j::Int, i::Int)` respectively (see [`SymbolicGA.getcomponent`](@ref)).

### Outputs

If you want to reconstruct a custom type from components, either define a constructor for a single tuple argument, e.g. `T(components::Tuple)`

=#

struct MyOutputType{T}
  values::Vector{T}
end

MyOutputType(x::Tuple) = MyOutputType(collect(x))

x = rand(3)
y = rand(3)

@ga 3 MyOutputType dual(x::1 ∧ y::1)

# If you don't want such constructor to be defined, you can overload `SymbolicGA.construct(::Type{T}, ::Tuple)` directly:

struct MyOutputType2{T}
  values::Vector{T}
end

SymbolicGA.construct(T::Type{<:MyOutputType2}, x::Tuple) = MyOutputType2(collect(x))

x = rand(3)
y = rand(3)

@ga 3 MyOutputType2 dual(x::1 ∧ y::1)

# Integrations for `Vector`, `Tuple` and `<:Real` have already been defined:

@ga 3 Tuple dual(x::1 ∧ y::1)

#-

@ga 3 Vector dual(x::1 ∧ y::1)

#-

z = rand(3)
@ga 3 Float16 dual(x::1 ∧ y::1 ∧ z::1)
