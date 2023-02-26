const GradeInfo = Union{Int,Vector{Int}}

@enum Head::UInt8 begin
  # Decorations.
  COMPONENT
  FACTOR
  BLADE
  KVECTOR
  MULTIVECTOR

  # Linear operations.
  ADDITION
  SUBTRACTION
  NEGATION # unused
  REVERSE
  ANTIREVERSE
  LEFT_COMPLEMENT
  RIGHT_COMPLEMENT

  # Products.
  GEOMETRIC_PRODUCT
  EXTERIOR_PRODUCT
  INTERIOR_PRODUCT
  COMMUTATOR_PRODUCT

  # Nonlinear operations.
  INVERSE
  EXPONENTIAL
end

const SCALAR_HEADS = [COMPONENT]

isscalar(head::Head) = head in SCALAR_HEADS

function Head(head::Symbol)
  head === :factor && return FACTOR
  head === :blade && return BLADE
  head === :kvector && return KVECTOR
  head === :multivector && return MULTIVECTOR
  head === :+ && return ADDITION
  head === :- && return SUBTRACTION
  head === :reverse && return REVERSE
  head === :antireverse && return ANTIREVERSE
  head === :left_complement && return LEFT_COMPLEMENT
  head === :right_complement && return RIGHT_COMPLEMENT
  head === :⟑ && return GEOMETRIC_PRODUCT
  head === :∧ && return EXTERIOR_PRODUCT
  head === :● && return INTERIOR_PRODUCT
  head === :× && return COMMUTATOR_PRODUCT
  head === :inverse && return INVERSE
  head === :exp && return EXPONENTIAL
  error("Head '$head' is unknown")
end

primitive type ID 64 end

ID(val::UInt64) = reinterpret(ID, val)
ID(val::Integer) = ID(UInt64(val))

mutable struct IDCounter
  val::UInt64
end

next!(counter::IDCounter) = ID(counter.val += 1)
IDCounter() = IDCounter(0)

mutable struct Expression
  head::Head
  grade::GradeInfo
  args::Vector{Union{Expression,ID}}
  cache
  function Expression(head::Head, args::AbstractVector, cache; simplify = true, grade = nothing)
    ex = new()
    ex.head = head
    ex.args = args
    ex.cache = cache::ExpressionCache
    !isnothing(grade) && (ex.grade = grade)
    !simplify && return ex
    simplify!(ex)::Expression
  end
end

const Term = Union{Expression, ID}

struct ExpressionSpec
  head::Head
  args::Vector{Term}
end

Base.:(==)(x::ExpressionSpec, y::ExpressionSpec) = x.head == y.head && x.args == y.args
Base.hash(spec::ExpressionSpec, h::UInt) = hash(hash(spec.head) + hash(spec.args), h)

struct Object
  val::Any
end

Base.:(==)(x::Object, y::Object) = typeof(x.val) == typeof(y.val) && x.val == y.val
Base.hash(x::Object, h::UInt) = hash(hash(x.val, hash(typeof(x.val))), h)

struct ExpressionCache
  sig::Signature
  counter::IDCounter
  primitive_ids::Dict{Object,ID}
  primitives::Dict{ID,Object}
  substitutions::Dict{ExpressionSpec,Expression}
end

Base.broadcastable(cache::ExpressionCache) = Ref(cache)

ExpressionCache(sig::Signature) = ExpressionCache(sig, IDCounter(), IdDict(), IdDict(), Dict())

is_expression_caching_enabled() = true

Base.getproperty(ex::Expression, field::Symbol) = field === :cache ? getfield(ex, field)::ExpressionCache : getfield(ex, field)

ExpressionSpec(cache::ExpressionCache, head::Head, args::AbstractVector) = ExpressionSpec(head, substitute_objects(cache, args))
ExpressionSpec(cache::ExpressionCache, head::Head, args...) = ExpressionSpec(cache, head, collect(Any, args))

Expression(head::Head, ex::Expression) = Expression(ex.cache, head, ex)
Expression(cache::ExpressionCache, head::Head, args...) = Expression(cache, ExpressionSpec(cache, head, args...))
function Expression(cache::ExpressionCache, spec::ExpressionSpec)
  haskey(cache.substitutions, spec) && is_expression_caching_enabled() && return cache.substitutions[spec]
  ex = Expression(spec.head, spec.args, cache)
  is_expression_caching_enabled() && (cache.substitutions[spec] = ex)
  ex
end

function substitute_objects(cache::ExpressionCache, args::AbstractVector)
  terms = Term[]
  for arg in args
    term = isa(arg, Expression) || isa(arg, ID) ? arg : begin
      id = get!(() -> next!(cache.counter), cache.primitive_ids, Object(arg))
      cache.primitives[id] = Object(arg)
      id
    end
    push!(terms, term)
  end
  terms
end

dereference(cache::ExpressionCache, primitive_id::ID) = cache.primitives[primitive_id].val
dereference(cache::ExpressionCache, x) = x
function dereference(ex::Expression)
  @assert length(ex) == 1
  inner = ex[1]
  isexpr(inner, COMPONENT) ? inner : dereference(ex.cache, inner::ID)
end

substitute!(ex::Expression, head::Head, args...) = substitute!(ex, ExpressionSpec(ex.cache, head, args...))
substitute!(ex::Expression, spec::ExpressionSpec) = substitute!(ex, Expression(ex.cache, spec))

function substitute!(ex::Expression, other::Expression)
  ex.head = other.head
  ex.args = other.args
  ex.grade = other.grade
  ex
end

function simplify!(ex::Expression)
  (; head, args, cache) = ex
  (; sig) = cache

  # Simplify nested factor expressions.
  head === FACTOR && isexpr(args[1], FACTOR) && return args[1]

  # Apply left and right complements.
  if head in (LEFT_COMPLEMENT, RIGHT_COMPLEMENT)
    isweightedblade(args[1]) && return substitute!(ex, weighted(Expression(cache, head, args[1][2]), args[1][1]))
    args[1] == factor(cache, 0) && return args[1]
  end
  head === LEFT_COMPLEMENT && isblade(args[1]) && return blade_left_complement(args[1])
  head === RIGHT_COMPLEMENT && isblade(args[1]) && return blade_right_complement(args[1])

  if head === BLADE
    # Sort basis vectors.
    if !issorted(args; by = x -> dereference(cache, x))
      perm = sortperm(args, by = x -> dereference(cache, x))
      fac = isodd(parity(perm)) ? -1 : 1
      return substitute!(ex, weighted(Expression(cache, BLADE, args[perm]), fac))
    end

    # Apply metric.
    if !allunique(args)
      last = nothing
      fac = 1
      i = 1
      while length(args) ≥ 2
        i > lastindex(args) && break
        new = dereference(cache, args[i])
        if !isnothing(last)
          if new == last
            m = metric(sig, last)
            iszero(m) && return substitute!(ex, scalar(cache, 0))
            fac *= m
            length(args) == 2 && return substitute!(ex, scalar(cache, fac))
            deleteat!(args, i)
            deleteat!(args, i - 1)
            i = max(i - 2, 1)
            last = i < firstindex(args) ? nothing : args[i]
          else
            last = new
          end
        else
          last = new
        end
        i += 1
      end
      return substitute!(ex, weighted(Expression(cache, BLADE, args), fac))
    end
  end

  if head === SUBTRACTION
    # Simplify unary minus operator to a multiplication with -1.
    length(args) == 1 && return substitute!(ex, weighted(args[1], -1))
    # Transform binary minus expression into an addition.
    @assert length(args) == 2
    return substitute!(ex, ADDITION, args[1], -args[2])
  end

  # Simplify whole expression to zero if a product term is zero.
  if head === GEOMETRIC_PRODUCT && any(isexpr(x, FACTOR) && dereference(x) == 0 for x in args)
    any(!isexpr(x, FACTOR) && !isexpr(x, BLADE, 0) for x in args) && @debug "Non-factor expression annihilated by a zero multiplication term" args
    return substitute!(ex, factor(cache, 0))
  end

  # Remove unit elements for multiplication and addition.
  did_remove = remove_unit_elements!(args, head)
  if did_remove
    head === GEOMETRIC_PRODUCT && isempty(args) && return substitute!(ex, scalar(cache, 1))
    head === ADDITION && isempty(args) && return substitute!(ex, scalar(cache, 0))
    return substitute!(ex, head, args)
  end

  if in(head, (GEOMETRIC_PRODUCT, ADDITION))
    length(args) == 1 && return substitute!(ex, args[1]::Expression)

    # Disassociate ⟑ and +.
    any(isexpr(head), args) && return substitute!(ex, disassociate1(args, head))
  end

  if head === GEOMETRIC_PRODUCT
    # Collapse factors into one and put it at the front.
    nf = count(isexpr(FACTOR), args)
    if nf ≥ 2
      # Collapse all factors into one at the front.
      factors, nonfactors = filter(isexpr(FACTOR), args), filter(!isexpr(FACTOR), args)
      fac = collapse_factors(cache, *, factors)
      length(args) == nf && return substitute!(ex, fac)
      return substitute!(ex, GEOMETRIC_PRODUCT, Term[fac; nonfactors])
    elseif nf == 1 && !isexpr(args[1], FACTOR)
      # Put the factor at the front.
      i = findfirst(isexpr(FACTOR), args)
      fac = args[i]::Expression
      deleteat!(args, i)
      pushfirst!(args, fac)
      return substitute!(ex, GEOMETRIC_PRODUCT, args)
    end

    # Collapse all bases and blades into a single blade.
    nb = count(isexpr(BLADE), args)
    if nb > 1
      n = length(args)
      blade_args = []
      for i in reverse(eachindex(args))
        x = args[i]
        isexpr(x, BLADE) || continue
        for arg in reverse(x.args)
          pushfirst!(blade_args, dereference(cache, arg)::Int)
        end
        deleteat!(args, i)
      end
      blade_ex = blade(cache, blade_args)
      # Return the blade if all the terms were collapsed.
      nb == n && return substitute!(ex, blade_ex)
      return substitute!(ex, GEOMETRIC_PRODUCT, Term[args; blade_ex])
    end
  end

  # Simplify addition of factors.
  head === ADDITION && all(isexpr(FACTOR), args) && return substitute!(ex, collapse_factors(cache, +, args))

  # Group blade components over addition.
  if head === ADDITION
    indices = findall(x -> isblade(x) || isweightedblade(x), args)
    if !isempty(indices)
      blades = args[indices]
      if !allunique(basis_vectors(b) for b in blades)
        blade_weights = Dict{Vector{Int},Expression}()
        for b in blades
          vecs = basis_vectors(b)
          weight = isweightedblade(b) ? b.args[1] : factor(cache, 1)
          blade_weights[vecs] = haskey(blade_weights, vecs) ? blade_weights[vecs] + weight : weight
        end
        for (vecs, weight) in blade_weights
          fac = dereference(weight)
          if Meta.isexpr(fac, :call) && fac.args[1] === :+
            terms = simplify_addition(@view fac.args[2:end])
            if isempty(terms)
              delete!(blade_weights, vecs)
            else
              blade_weights[vecs] = factor(cache, length(terms) == 1 ? terms[1] : Expr(:call, :+, terms...))
            end
          end
        end
        new_args = args[setdiff(eachindex(args), indices)]
        append!(new_args, Expression(cache, GEOMETRIC_PRODUCT, weight, blade(cache, vecs)) for (vecs, weight) in blade_weights)
        return substitute!(ex, ADDITION, new_args)
      end
    end
  end


  if head === FACTOR
    fac = dereference(cache, only(args))

    # Disassociate `Expr` multiplications and additions.
    if iscall(fac, :+) || iscall(fac, :*)
      disassociated = disassociate1(fac)
      fac ≠ disassociated && return substitute!(ex, FACTOR, disassociated)
    end
    if iscall(fac, :*)
      distributed = distribute1(fac)
      fac ≠ distributed && return substitute!(ex, FACTOR, distributed)
    end
    if iscall(fac, :+)
      terms = simplify_addition(args)
      terms !== args && return substitute!(ex, FACTOR, isempty(terms) ? 0 : length(terms) == 1 ? terms[1] : Expr(:call, :+, terms...))
    end

    # Simplify -1 factors.
    new_fac = simplify_negations(fac)
    fac !== new_fac && return substitute!(ex, factor(cache, new_fac))
  end

  # Check that arguments make sense.
  n = expected_nargs(head)
  !isnothing(n) && @assert length(args) == n "Expected $n arguments for expression $head, $(length(args)) were provided\nArguments: $args"
  @assert !isempty(args) || head === BLADE
  head === BLADE && @assert all(isa(dereference(cache, i), Int) for i in args) "Expected integer arguments in BLADE expression, got $(dereference.(cache, ex))"
  head === FACTOR && @assert !isa(args[1], Expression) || isscalar(args[1].head) "`Expression` argument detected in FACTOR expression: $(args[1])"

  # Propagate complements over addition.
  head in (LEFT_COMPLEMENT, RIGHT_COMPLEMENT) && isexpr(args[1], ADDITION) && return substitute!(ex, ADDITION, Expression.(cache, head, args[1]))

  # Distribute products over addition.
  head in (GEOMETRIC_PRODUCT, EXTERIOR_PRODUCT, INTERIOR_PRODUCT, COMMUTATOR_PRODUCT) && any(isexpr(arg, ADDITION) for arg in args) && return substitute!(ex, distribute1(args, head))

  # Eagerly apply reversions.
  head in (REVERSE, ANTIREVERSE) && return substitute!(ex, apply_reverse_operators(ex))

  # Expand common operators.
  # ========================

  # The exterior product is associative, no issues there.
  if head === EXTERIOR_PRODUCT
    n = sum(x -> grade(x::Expression)::Int, args)
    return substitute!(ex, project!(Expression(cache, GEOMETRIC_PRODUCT, args), n))
  end

  if head === INTERIOR_PRODUCT
    # The inner product must have only two operands, as no associativity law is available to derive a canonical binarization.
    # Homogeneous vectors are expected, so the grade should be known.
    r, s = grade(args[1]::Expression)::Int, grade(args[2]::Expression)::Int
    if (iszero(r) || iszero(s))
      (!iszero(r) || !iszero(s)) && @debug "Non-scalar expression annihilated in inner product with a scalar"
      return scalar(cache, 0)
    end
    return substitute!(ex, project!(Expression(cache, GEOMETRIC_PRODUCT, args), abs(r - s)))
  end

  if head === COMMUTATOR_PRODUCT
    # The commutator product must have only two operands, as no associativity law is available to derive a canonical binarization.
    a, b = args[1]::Expression, args[2]::Expression
    return substitute!(ex, weighted(Expression(cache, SUBTRACTION, Expression(cache, GEOMETRIC_PRODUCT, a, b), Expression(cache, GEOMETRIC_PRODUCT, b, a)), 0.5))
  end

  if head === EXPONENTIAL
    # Find a closed form for the exponentiation, if available.
    a = args[1]::Expression
    (isblade(a) || isweightedblade(a)) && return substitute!(ex, expand_exponential(a))
    # return expand_exponential(sig::Signature, a)
    isexpr(a, ADDITION) && return substitute!(ex, GEOMETRIC_PRODUCT, Expression.(cache, EXPONENTIAL, a)...)
    throw(ArgumentError("Exponentiation is not supported for non-blade elements."))
  end

  if head === INVERSE
    a = args[1]::Expression
    isexpr(a, FACTOR) && return substitute!(ex, factor(cache, scalar_inverse(dereference(a))))
    isweightedblade(a, 0) && return substitute!(ex, scalar(cache, Expression(cache, INVERSE, a[1]::Expression)))
    isblade(a, 0) && return a
    sc = Expression(cache, GEOMETRIC_PRODUCT, Expression(cache, REVERSE, a), a)
    isscalar(sc) || error("`reverse(A) ⟑ A` is not a scalar, suggesting that A is not a versor. Inversion is only supported for versors at the moment.")
    return substitute!(ex, GEOMETRIC_PRODUCT, Expression(cache, REVERSE, a), Expression(cache, INVERSE, sc))
  end

  ex.grade = infer_grade(cache, head, args)::GradeInfo

  ex
end

iscall(ex, f) = Meta.isexpr(ex, :call) && ex.args[1] === f

function remove_unit_elements!(args, head)
  n = length(args)
  if head === GEOMETRIC_PRODUCT
    filter!(x -> !isexpr(x, FACTOR) || dereference(x) != 1, args)
  elseif head === ADDITION
    filter!(x -> !isexpr(x, FACTOR) || dereference(x) != 0, args)
  end
  did_remove = n > length(args)
  did_remove
end

function collapse_factors(cache, f::F, args) where {F<:Function}
  @assert f === (*) || f === (+)
  unit = f === (*) ? one : zero
  isunit = f === (*) ? isone : iszero
  opaque_factors = Any[]
  value = unit(Int64)
  for fac in args
    if isopaquefactor(fac)
      push!(opaque_factors, dereference(fac))
    else
      value = f(value, dereference(fac))
    end
  end

  if !isempty(opaque_factors)
    flattened_factors = Any[]
    for fac in opaque_factors
      if Meta.isexpr(fac, :call) && fac.args[1] === Symbol(f)
        append!(flattened_factors, fac.args[2:end])
      else
        push!(flattened_factors, fac)
      end
    end
    !isunit(value) && pushfirst!(flattened_factors, value)
    length(flattened_factors) == 1 && return factor(cache, flattened_factors[1])

    ex = Expr(:call)
    ex.args = flattened_factors
    pushfirst!(ex.args, Symbol(f))
    return factor(cache, ex)
  end
  return factor(cache, value)
end

function disassociate1(args, op::Head)
  (; cache) = args[1]::Expression
  new_args = []
  for arg in args
    if isexpr(arg, op)
      append!(new_args, arg.args)
    else
      push!(new_args, arg)
    end
  end
  Expression(cache, op, new_args)
end

function disassociate1(ex::Expr)
  f = ex.args[1]
  res = Expr(:call, f)
  for arg in @view ex.args[2:end]
    iscall(arg, f) ? append!(res.args, arg.args[2:end]) : push!(res.args, arg)
  end
  res
end

function distribute1(args, op::Head)
  x, ys = args[1], @view args[2:end]
  (; cache) = x
  base = isexpr(x, ADDITION) ? x.args : [x]
  for y in ys
    new_base = []
    yterms = isexpr(y, ADDITION) ? y.args : (y,)
    for xterm in base
      for yterm in yterms
        push!(new_base, Expression(cache, op, xterm, yterm))
      end
    end
    base = new_base
  end
  Expression(cache, ADDITION, base)
end

function distribute1(ex::Expr)
  (; args) = ex
  x, ys = args[2], @view args[3:end]
  base = iscall(x, :+) ? x.args[2:end] : [x]
  for y in ys
    new_base = []
    yterms = iscall(y, :+) ? y.args[2:end] : (y,)
    for xterm in base
      for yterm in yterms
        push!(new_base, disassociate1(Expr(:call, :*, xterm, yterm)))
      end
    end
    base = new_base
  end
  length(base) == 1 && return only(base)
  res = Expr(:call, :+)
  append!(res.args, base)
  res
end

function simplify_negations(fac)
  if Meta.isexpr(fac, :call) && fac.args[1] === :*
    head, args... = fac.args
    n = count(x -> x == -1, args)
    if n > 1
      filter!(≠(-1), args)
      isodd(n) && pushfirst!(args, -1)
      isempty(args) && return 1
      length(args) == 1 && return args[1]
      return Expr(:call, head, args...)
    end
  end
  fac
end

function simplify_addition(args)
  counter = 0
  ids = Dictionary{Any,Int}()
  id_counts = Dictionary{Int,Int}()
  new_args = []
  for arg in args
    n = 1
    obj = arg
    if Meta.isexpr(arg, :call) && arg.args[1] === :*
      xs = arg.args[2:end]
      @assert length(xs) > 1
      filter!(xs) do y
        if isa(y, Int) || isa(y, Integer)
          n *= y
          return false
        end
        true
      end
      obj = isempty(xs) ? 1 : length(xs) == 1 ? xs[1] : sort!(xs; by = objectid)
    end
    id = get!(() -> (counter += 1), ids, obj)
    set!(id_counts, id, something(get(id_counts, id, nothing), 0) + n)
  end
  for (x, id) in pairs(ids)
    n = id_counts[id]
    n == 0 && continue
    isa(x, Vector{Any}) && (x = Expr(:call, :*, x...))
    if n == 1
      push!(new_args, x)
    else
      # TODO: remove the need for manual disassociation.
      push!(new_args, disassociate1(scalar_multiply(n, x)))
    end
  end
  new_args == args ? args : new_args
end

function expected_nargs(head)
  in(head, (FACTOR, NEGATION, REVERSE, ANTIREVERSE, LEFT_COMPLEMENT, RIGHT_COMPLEMENT, INVERSE, EXPONENTIAL)) && return 1
  in(head, (INTERIOR_PRODUCT, COMMUTATOR_PRODUCT, SUBTRACTION)) && return 2
  nothing
end

function infer_grade(cache, head::Head, args)
  head === FACTOR && return 0
  head === COMPONENT && return 0
  head === BLADE && return count(x -> isodd(dereference(cache, x)), map(x -> count(==(x), args), unique(args)))
  if head === GEOMETRIC_PRODUCT
    @assert length(args) == 2 && isexpr(args[1], FACTOR)
    return grade(args[2]::Expression)
  end
  if in(head, (ADDITION, MULTIVECTOR))
    gs = Int64[]
    for arg in args
      append!(gs, grade(arg))
    end
    sort!(unique!(gs))
    length(gs) == 1 && return first(gs)
    return gs
  end
  if head === KVECTOR
    grades = map(args) do arg
      isa(arg, Expression) || error("Expected argument of type $Expression, got $arg")
      g = arg.grade
      isa(g, Int) || error("Expected grade to be known for argument $arg in $head expression")
      g
    end
    unique!(grades)
    length(grades) == 1 || error("Expected unique grade for k-vector expression, got grades $grades")
    return first(grades)
  end
  error("Cannot infer grade for unexpected $head expression with arguments $args")
end

# Empirical formula.
# If anyone has a better one, please let me know.
function blade_right_complement(b::Expression)
  (; cache) = b
  (; sig) = cache
  blade(cache, reverse!(setdiff(1:dimension(sig), basis_vectors(b)))) * factor(cache, (-1)^(
    isodd(sum(basis_vectors(b); init = 0)) +
    (dimension(sig) ÷ 2) % 2 +
    isodd(dimension(sig)) & isodd(length(b))
  ))
end

# Exact formula derived from the right complement.
blade_left_complement(b::Expression) = factor(b.cache, (-1)^(grade(b) * antigrade(b))) * blade_right_complement(b)

function project!(ex, g, level = 0)
  isa(ex, Expression) || return ex
  if all(isempty(intersect(g, g′)) for g′ in grade(ex))
    iszero(level) && @debug "Non-scalar expression annihilated in projection into grade(s) $g" ex
    return factor(ex.cache, 0)
  end
  if isexpr(ex, ADDITION)
    for (i, x) in enumerate(ex)
      ex.args[i] = project!(x, g, level + 1)
    end
    return simplify!(ex)
  end
  ex
end

function apply_reverse_operators(ex::Expression)
  (; cache) = ex
  (; sig) = cache
  orgex = ex
  prewalk(ex) do ex
    isexpr(ex, (REVERSE, ANTIREVERSE)) || return ex
    reverse_op = ex.head
    anti = reverse_op === ANTIREVERSE
    ex = ex[1]
    @assert isa(ex, Expression) "`Expression` argument expected for `$reverse_op`."
    # Distribute over addition.
    isexpr(ex, (ADDITION, KVECTOR, MULTIVECTOR)) && return Expression(cache, ex.head, anti ? antireverse.(ex) : reverse.(ex))
    !anti && in(ex.grade, (0, 1)) && return ex
    anti && in(antigrade(sig, ex.grade), (0, 1)) && return ex
    isexpr(ex, GEOMETRIC_PRODUCT) && return propagate_reverse(reverse_op, ex)
    @assert isexpr(ex, BLADE) "Unexpected operator $(ex.head) encountered when applying $reverse_op operators."
    n = anti ? antigrade(ex)::Int : grade(ex)::Int
    fac = (-1)^(n * (n - 1) ÷ 2)
    isone(fac) ? ex : -ex
  end
end

# grade(x) + antigrade(x) == dimension(sig)
antigrade(sig::Signature, g::Int) = dimension(sig) - g
antigrade(sig::Signature, g::Vector{Int}) = antigrade.(sig, g)
antigrade(ex::Expression) = antigrade(ex.cache.sig, ex.grade)

function propagate_reverse(reverse_op, ex::Expression)
  (; cache) = ex
  res = Term[]
  for arg in ex
    arg::Expression
    skip = isexpr(arg, FACTOR) || reverse_op === REVERSE ? in(arg.grade, (0, 1)) : in(antigrade(cache.sig, arg.grade), (0, 1))
    skip ? push!(res, arg) : push!(res, Expression(cache, reverse_op, arg))
  end
  Expression(cache, ex.head, res)
end

function expand_exponential(b::Expression)
  (; cache) = b
  (; sig) = cache
  vs = basis_vectors(b)
  is_degenerate(sig) && any(iszero(metric(sig, v)) for v in vs) && return Expression(cache, ADDITION, scalar(cache, 1), b)
  b² = Expression(cache, GEOMETRIC_PRODUCT, b, b)
  @assert isscalar(b²)
  α² = isweightedblade(b²) ? dereference(b²[1]) : 1
  α = scalar_sqrt(scalar_abs(α²))
  # The sign may not be deducible from α² directly, so we compute it manually given metric simplifications and blade permutations.
  is_negative = isodd(count(metric(sig, v) == -1 for v in vs)) ⊻ iseven(length(vs))
  # Negative square: cos(α) + A * sin(α) / α
  # Positive square: cosh(α) + A * sinh(α) / α
  (s, c) = is_negative ? (scalar_sin, scalar_cos) : (scalar_sinh, scalar_cosh)
  ex = Expression(cache, ADDITION, scalar(cache, c(α)), Expression(cache, GEOMETRIC_PRODUCT, scalar(cache, scalar_nan_to_zero(scalar_divide(s(α), α))), b))
  ex
end

scalar_exponential(x::Number) = exp(x)
scalar_exponential(x) = :($exp($x))
scalar_multiply(x::Number, y::Number) = x * y
scalar_multiply(x, y) = :($x * $y)
scalar_divide(x, y) = scalar_multiply(x, scalar_inverse(y))
scalar_nan_to_zero(x::Number) = ifelse(isnan(x), 0, x)
scalar_nan_to_zero(x) = :(ifelse(isnan($x), 0, $x))
scalar_inverse(x::Number) = inv(x)
scalar_inverse(x) = :($inv($x))
scalar_cos(x::Number) = cos(x)
scalar_cos(x) = :($cos($x))
scalar_sin(x::Number) = sin(x)
scalar_sin(x) = :($sin($x))
scalar_cosh(x::Number) = cosh(x)
scalar_cosh(x) = :($cosh($x))
scalar_sinh(x::Number) = sinh(x)
scalar_sinh(x) = :($sinh($x))
scalar_sqrt(x::Number) = sqrt(x)
scalar_sqrt(x) = :($sqrt($x))
scalar_abs(x::Number) = abs(x)
scalar_abs(x) = :($abs($x))

# Basic interfaces.

Base.:(==)(x::Expression, y::Expression) = x.head == y.head && (!isdefined(x, :grade) || !isdefined(y, :grade) || x.grade == y.grade) && x.args == y.args
Base.getindex(x::Expression, args...) = x.args[args...]
Base.firstindex(x::Expression) = firstindex(x.args)
Base.lastindex(x::Expression) = lastindex(x.args)
Base.length(x::Expression) = length(x.args)
Base.iterate(x::Expression, args...) = iterate(x.args, args...)
Base.keys(x::Expression) = keys(x.args)
Base.view(x::Expression, args...) = view(x.args, args...)

# Introspection utilities.

isexpr(ex, head::Head) = isa(ex, Expression) && ex.head === head
isexpr(ex, heads) = isa(ex, Expression) && in(ex.head, heads)
isexpr(ex, head::Head, n::Int) = isa(ex, Expression) && isexpr(ex, head) && length(ex.args) == n
isexpr(heads) = ex -> isexpr(ex, heads)
grade(ex::Expression) = ex.grade::GradeInfo
isblade(ex, n = nothing) = isexpr(ex, BLADE) && (isnothing(n) || n == length(ex))
isscalar(ex::Expression) = isblade(ex, 0) || isweightedblade(ex, 0)
isweighted(ex) = isexpr(ex, GEOMETRIC_PRODUCT, 2) && isexpr(ex[1]::Expression, FACTOR)
isweightedblade(ex, n = nothing) = isweighted(ex) && isblade(ex[2]::Expression, n)
isopaquefactor(ex) = isexpr(ex, FACTOR) && isa(dereference(ex), Union{Symbol, Expr, QuoteNode, Expression})

function basis_vectors(ex::Expression)
  isweightedblade(ex) && return basis_vectors(ex[2]::Expression)
  isexpr(ex, BLADE) && return [dereference(ex.cache, i)::Int for i in ex]
  error("Expected blade or weighted blade expression, got $ex (head: $(ex.head))")
end

function lt_basis_order(xinds, yinds)
  nx, ny = length(xinds), length(yinds)
  nx ≠ ny && return nx < ny
  for (xi, yi) in zip(xinds, yinds)
    xi ≠ yi && return xi < yi
  end
  false
end

# Helper functions.

factor(cache::ExpressionCache, x) = Expression(cache, FACTOR, x)
weighted(x::Expression, fac) = Expression(x.cache, GEOMETRIC_PRODUCT, factor(x.cache, fac), x)
scalar(cache::ExpressionCache, fac) = factor(cache, fac) * blade(cache)
blade(cache::ExpressionCache, xs...) = Expression(cache, BLADE, xs...)
kvector(args::AbstractVector) = Expression((args[1]::Expression).cache, KVECTOR, args)
kvector(xs...) = kvector(collect(Term, xs))
multivector(args::AbstractVector) = Expression((args[1]::Expression).cache, MULTIVECTOR, args)
multivector(xs...) = multivector(collect(Term, xs))
Base.reverse(ex::Expression) = Expression(REVERSE, ex)
antireverse(ex::Expression) = Expression(ANTIREVERSE, ex)
antiscalar(cache::ExpressionCache, fac) = factor(cache, fac) * antiscalar(cache)
antiscalar(cache::ExpressionCache) = blade(cache, 1:dimension(cache.sig))

⟑(x::Expression, args::Expression...) = Expression(x.cache, GEOMETRIC_PRODUCT, x, args...)
Base.:(*)(x::Expression, args::Expression...) = Expression(x.cache, GEOMETRIC_PRODUCT, x, args...)
Base.:(*)(x::Number, y::Expression, args::Expression...) = *(factor(y.cache, x), y, args...)
Base.:(+)(x::Expression, args::Expression...) = Expression(x.cache, ADDITION, x, args...)
Base.:(-)(x::Expression, args::Expression...) = Expression(x.cache, SUBTRACTION, x, args...)
∧(x::Expression, y::Expression) = Expression(x.cache, EXTERIOR_PRODUCT, x, y)
exterior_product(x::Expression, y::Expression) = Expression(x.cache, EXTERIOR_PRODUCT, x, y)

# Traversal/transformation utilities.

walk(ex::Expression, inner, outer) = outer(Expression(ex.cache, ex.head, Any[inner(x) for x in ex if !isnothing(x)]))
walk(ex, inner, outer) = outer(ex)
postwalk(f, ex) = walk(ex, x -> postwalk(f, x), f)
prewalk(f, ex) = walk(f(ex), x -> prewalk(f, x), identity)

function traverse(f, ex, ::Type{T} = Expression) where {T}
  f(ex) === false && return nothing
  !isa(ex, T) && return nothing
  for arg in ex.args
    traverse(f, arg, T)
  end
end

# Display.

function Base.show(io::IO, ex::Expression)
  isexpr(ex, BLADE) && return print(io, 'e', join(subscript.(dereference.(ex.cache, ex))))
  isexpr(ex, FACTOR) && return print_factor(io, ex)
  if isexpr(ex, GEOMETRIC_PRODUCT) && isexpr(ex[end], BLADE)
    if length(ex) == 2 && isexpr(ex[1], FACTOR)
      fac = ex[1]
      if isexpr(fac, COMPONENT) || !Meta.isexpr(dereference(fac), :call)
        return print(io, fac, " ⟑ ", ex[end])
      end
    else
      return print(io, '(', join(ex[1:(end - 1)], " ⟑ "), ") ⟑ ", ex[end])
    end
  end
  isexpr(ex, (GEOMETRIC_PRODUCT, ADDITION, MULTIVECTOR)) && return print(io, '(', Expr(:call, head_symbol(ex.head), ex.args...), ')')
  isexpr(ex, KVECTOR) && return print(io, Expr(:call, Symbol("kvector", subscript(ex.grade)), ex.args...))
  print(io, Expression, "(:", ex.head, ", ", join(ex.args, ", "), ')')
end

function head_symbol(head::Head)
  head === GEOMETRIC_PRODUCT && return :⟑
  head === ADDITION && return :+
  head === MULTIVECTOR && return :multivector
end

function print_factor(io, ex::Expression)
  if isexpr(ex, COMPONENT)
    # If we got one argument `x`, print `x[]`, unless it is a literal, in which case print e.g. `3`.
    # Otherwise, print `x[indices...]`.
    x, args... = dereference.(ex.cache, ex)
    length(ex) == 1 && return print(io, isa(x, Number) ? x : Expr(:ref, x))
    return print(io, Expr(:ref, x, args...))
  end
  print_factor(io, dereference(ex))
end

function print_factor(io, ex)
  Meta.isexpr(ex, :call) && in(ex.args[1], (:*, :+)) && return print(io, join((sprint(print_factor, arg) for arg in @view ex.args[2:end]), " $(ex.args[1]) "))
  print(io, ex)
end

"""
Return `val` as a subscript, used for printing `UnitBlade` and `Blade` instances.
"""
function subscript(val)
  r = div(val, 10)
  subscript_char(x) = Char(8320 + x)
  r > 0 ? string(subscript_char(r), subscript_char(mod(val, 10))) : string(subscript_char(val))
end
