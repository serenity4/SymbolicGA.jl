function sum_homogeneous(k, bs::AbstractVector{Blade{T}}) where {T}
    kvec = zero(KVector{k}, T)
    for b ∈ bs
        add!(kvec, b)
    end
    kvec
end

sum_homogeneous(k, bs::Blade{T}...) where {T} = sum_homogeneous(k, collect(bs))

function (+)(bs::Blade{T}...) where {T}
    grades = unique(grade.(bs))
    if length(grades) == 1
        k = grades[1]
        k == 0 && return scalar(reduce(+, getproperty.(bs, :coef)))
        return sum_homogeneous(k, SVector{length(bs)}(bs))
    else
        mvec = zero(Multivector, T)
        for b ∈ bs
            add!(mvec, b)
        end
        return mvec
    end
end

@commutative (+)(b::Blade, kvec::KVector) = +(b, blades(kvec)...)
@commutative (+)(b::Blade, mv::Multivector) = +(b, blades(mv)...)

convert(T::Type{<:Blade}, b::Blade) = T(b.coef, b.index)
convert(T::Type{<:Blade}, x::Number) = scalar(x)

promote_rule(T1::Type{<:Blade}, T2::Type{<:Blade}) = Blade{promote_type(eltype(T1), eltype(T2))}
promote_rule(T1::Type{<:KVector}, T2::Type{<:KVector}) = Multivector
promote_rule(T1::Type{<:Multivector}, T2::Type{<:KVector}) = Multivector
# promote_rule(T1::Type{<:Blade}, T2::Type{<:Number}) = Blade{promote_type(eltype(T1), T2)}

(+)(kvecs::KVector{K}...) where {K} = KVector{K}(.+(getproperty.(kvecs, :coefs)...))
(+)(mvecs::Multivector...) = Multivector(.+(getproperty.(mvecs, :coefs)...))
(+)(x::GeometricAlgebraType, y::GeometricAlgebraType) = +(promote(x, y)...)
@type_commutative (+)(x::GeometricAlgebraType, y::Number) = +(promote(x, y)...)

sum(x::AbstractVector{<:GeometricAlgebraType}) = reduce(+, x)

(-)(x::Blade) = Blade(-x.coef, x.index)
(-)(x::Multivector) = Multivector(map(-, x.coefs))
(-)(x::KVector) = typeof(x)(map(-, x.coefs))
(-)(x::GeometricAlgebraType, y::GeometricAlgebraType) = x + (-y)

"""
    x ∧ y
Outer product of `x` with `y`.
"""
function ∧ end

"""
    x ⋅ y
Inner product of `x` with `y`. This product is in general non-associative, and is conventinally executed right to left in absence of parenthesis.
For example, `A ⋅ B ⋅ C == A ⋅ (B ⋅ C)`, and in most cases, `A ⋅ B ⋅ C ≠ (A ⋅ B) ⋅ C`.
"""
function ⋅ end

"""
    x ⦿ y
Scalar product between `x` and `y`.
"""
function ⦿ end

"""
    x ∨ y
Meet of `x` and `y`
"""
function ∨ end

"""
    lcontract(x, y)
Left contraction of `x` with `y`.
"""
function lcontract end

"""
    rcontract(x, y)
Right contraction of `x` with `y`.
"""
function rcontract end

function geom end

"""
Duality operator.
"""
function dual end

geom!(res, args...) = add!(res, geom(args...))

@generated op_result_type(op, T1::Type{<:KVector}, T2::Type{<:KVector}) = KVector{result_grade(op, grade(T1), grade(T2)), promote_type(vectype(T1), vectype(T2))}

(*)(x::Blade, y::Blade) = geom(x, y)

function (*)(x::GeometricAlgebraType, y::GeometricAlgebraType)
    res = zero(op_result_type(*, x, y))
    geom!(res, x, y)
    res
end

function geom!(res::Multivector, x::Blade, y::KVector)
    for b ∈ blades(y)
        geom!(res, x, b)
    end
end

function geom!(res::Multivector, x::KVector, y::Blade)
    for b ∈ blades(x)
        geom!(res, b, y)
    end
end

function geom!(res::Multivector, x::KVector, y::KVector)
    for b ∈ blades(x)
        geom!(res, b, y)
    end
end

function geom!(res::Multivector, x::Multivector, y)
    for kvec ∈ blades(x)
        geom!(res, kvec, y)
    end
end

function geom!(res::Multivector, x::Multivector, y::Multivector)
    for kvec ∈ blades(y)
        geom!(res, x, kvec)
    end
end

@commutative (*)(x::KVector, y::Number) = typeof(x)(x.coefs .* y, x.start)
@commutative (*)(x::Multivector, y::Number) = typeof(x)(x.coefs .* y)
@commutative (*)(x::Blade, y::Number) = Blade(x.coef * y, x.index)
(*)(xs::GeometricAlgebraType...) = reduce(*, xs)

(^)(x::GeometricAlgebraType, p::Integer) = prod(ntuple(i -> x, p))

@commutative (⋅)(x::GeometricAlgebraType, y::Number) = scalar(zero(promote_type(eltype(x), typeof(y))))
(⋅)(x::GeometricAlgebraType, y::Blade) = is_scalar(y) ? x ⋅ y.coef : grade_projection(x * y, Val(result_grade(⋅, grade(x), grade(y))))
(⋅)(x::Multivector, y::Blade) = sum(kx ⋅ y for kx ∈ kvectors(x))
(⋅)(x::Blade, y::GeometricAlgebraType) = is_scalar(x) ? x.coef ⋅ y : grade_projection(x * y, Val(result_grade(⋅, grade(x)), grade(x)))
(⋅)(x::Blade, y::Multivector) = sum(x ⋅ ky for ky ∈ kvectors(y))
(⋅)(x::Blade, y::Blade) = is_scalar(x) || is_scalar(y) ? scalar(zero(promote_type(eltype(x), eltype(y)))) : grade_projection(x * y, Val(result_grade(⋅, grade(x), grade(y))))
(⋅)(x::T, y::T) where {T<:Number} = scalar(zero(T))
(⋅)(x::Number, y::Number) = scalar(zero(promote_type(typeof(x), typeof(y))))

(⦿)(x::Number, y::Number) = x * y

@commutative (∧)(x::GeometricAlgebraType, y::Number) = x * y
(∧)(x::Number, y::Number) = scalar(x * y)
(×)(x, y) = (x * y - y * x) / 2

for op ∈ [:∧, :⋅, :⦿]

    # ⋅ is not associative
    fold = op == :⋅ ? foldr : reduce

    if op == :⦿
        @eval begin
            ($op)(x::GeometricAlgebraType, y::GeometricAlgebraType) = first(grade_projection(x * y, Val(0)).coefs)
            ($op)(x::Blade, y::Blade) = grade_projection(x * y, Val(0)).coef
        end
    else
        @eval ($op)(x::GeometricAlgebraType, y::GeometricAlgebraType) = grade_projection(x * y, Val(result_grade($op, grade(x), grade(y))))
    end

    @eval begin
        ($op)(x::Any, y::Any, z::Any...) = $fold($op, vcat(x, y, z...))
        ($op)(x::Multivector, y::Multivector) = sum($op(kx, ky) for kx ∈ kvectors(x), ky ∈ kvectors(y))
        ($op)(x::GeometricAlgebraType, y::Multivector) = sum($op(x, ky) for ky ∈ kvectors(y))
        ($op)(x::Multivector, y::GeometricAlgebraType) = sum($op(kx, y) for kx ∈ kvectors(x))
    end
end

permsign(i, j) =
    1 - 2 * parity(sortperm(SVector{length(i) + length(j),Int}(vcat(i, j))))

reverse_sign(grade) = (-1) ^ (grade * (grade - 1) ÷ 2)

reverse(x::GeometricAlgebraType) = x * reverse_sign(grade(x))

function grade_projection(x::Multivector, ::Val{K}) where {K}
    K > log2(length(x)) && return scalar(zero(eltype(x)))
    res = zero(KVector{K}, eltype(x))
    res.coefs .= x.coefs[indices(res)]
    res
end

grade_projection(x::KVector{K}, ::Val{K}) where {K} = x

grade_projection(x::KVector, ::Val{K}) where {K} = zero(KVector{K}, eltype(x))

magnitude2(x) = x ⦿ x

magnitude(x) = sqrt(abs(magnitude2(x)))

inv(x::GeometricAlgebraType) = x / magnitude2(x)

(/)(x::GeometricAlgebraType, y::GeometricAlgebraType) = x * inv(y)
@type_commutative (/)(x::Any, y::GeometricAlgebraType) = x * inv(y)

"""
Return the grade(s) that can be present in the result of an operation.
"""
result_grade(::typeof(⋅), r, s) = abs(r - s)
result_grade(::typeof(∧), r, s) = r + s
result_grade(::typeof(lcontract), r, s) = s - r
result_grade(::typeof(rcontract), r, s) = r - s
result_grade(::typeof(⦿), _, _) = 0
result_grade(::typeof(*), r, s) = result_grade(⋅, r, s):2:result_grade(∧, r, s)
