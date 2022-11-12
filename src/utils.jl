"""
    @type_commutative 2 3 f(op, x::Type1, y::Type2) = ...

Wrap a method definition, resulting in the definition of another method
which is identical except that the types of x and y are swapped.
Note that, any expression valid for the type of x must be valid for the type of y,
since the body of the function stays exactly the same.
"""
macro type_commutative(ix, iy, expr)
    additional_decl = deepcopy(expr)

    fargs = extract_fargs(ix, iy, additional_decl)
    ix, iy = handle_ix_iy(ix, iy, fargs)

    if length(fargs[ix].args) == length(fargs[iy].args) == 1
        fargs[ix].args[1], fargs[iy].args[1] = fargs[iy].args[1], fargs[ix].args[1]
    else
        fargs[ix].args[2], fargs[iy].args[2] = fargs[iy].args[2], fargs[ix].args[2]
    end

    quote
        $(esc(expr))
        $(esc(additional_decl))
    end
end

macro type_commutative(expr) :(@type_commutative($nothing, $nothing, $expr)) end

"""
    @commutative 2 3 f(op, x::Type1, y::Type2) = ...

Wrap a method definition, resulting in the definition of another method
which is identical except that x and y are swapped.
"""
macro commutative(ix, iy, expr)
    additional_decl = deepcopy(expr)

    fargs = extract_fargs(ix, iy, additional_decl)
    ix, iy = handle_ix_iy(ix, iy, fargs)

    fargs[ix], fargs[iy] = fargs[iy], fargs[ix]

    quote
        $(esc(expr))
        $(esc(additional_decl))
    end
end

macro commutative(expr) :(@commutative($nothing, $nothing, $expr)) end

function extract_fargs(ix, iy, decl)
    fdecl, _ = decl.args
    if fdecl.head == :where
        fdecl = first(fdecl.args)
    end
    
    fargs = @view fdecl.args[2:end]
    @assert length(fargs) >= 2 "The method must have at least two arguments"
    
    fargs
end

function handle_ix_iy(ix, iy, fargs)
    if isnothing(ix) || isnothing(iy)
        @assert length(fargs) == 2 "`ix` and `iy` must be provided for a method with more than 2 arguments"
        ix = 1
        iy = 2
    end

    ix, iy
end

"""
Return `val` as a subscript, used for printing `UnitBlade` and `Blade` instances.
"""
function subscript(val)
    r = div(val, 10)
    subscript_char(x) = Char(8320 + x)
    r > 0 ? string(subscript_char(r), subscript_char(mod(val, 10))) : string(subscript_char(val))
end
