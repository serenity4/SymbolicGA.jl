const GradeInfo = Union{Int,Vector{Int}}

@enum Head::UInt8 begin
  # Algebraic elements.
  SCALAR = 0
  BLADE = 1
  KVECTOR = 2
  MULTIVECTOR = 3

  # Linear operations.
  ADDITION = 10
  SUBTRACTION = 11
  NEGATION = 12 # unused
  REVERSE = 13
  ANTIREVERSE = 14
  LEFT_COMPLEMENT = 15
  RIGHT_COMPLEMENT = 16

  # Products.
  GEOMETRIC_PRODUCT = 20
  EXTERIOR_PRODUCT = 21
  INTERIOR_PRODUCT = 22
  COMMUTATOR_PRODUCT = 23

  # Nonlinear operations.
  INVERSE = 30
  EXPONENTIAL = 31

  # Scalar operations.
  SCALAR_ADDITION = 60
  SCALAR_EXPONENTIAL = 61
  SCALAR_PRODUCT = 62
  SCALAR_DIVISION = 63
  SCALAR_NAN_TO_ZERO = 64
  SCALAR_INVERSE = 65
  SCALAR_COS = 66
  SCALAR_SIN = 67
  SCALAR_COSH = 68
  SCALAR_SINH = 69
  SCALAR_SQRT = 70
  SCALAR_ABS = 71
  SCALAR_COMPONENT = 72 
  SCALAR_NEGATION = 73
  SCALAR_SUBTRACTION = 74
end

isscalar(head::Head) = head === SCALAR || UInt8(head) ≥ 60

function Head(head::Symbol)
  head === :scalar && return SCALAR
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

function promote_scalar(head::Head)
  head === ADDITION && return SCALAR_ADDITION
  head === NEGATION && return SCALAR_NEGATION
  head === INVERSE && return SCALAR_INVERSE
  head === SUBTRACTION && return SCALAR_SUBTRACTION
  head === EXPONENTIAL && return SCALAR_EXPONENTIAL
  head === GEOMETRIC_PRODUCT && return SCALAR_PRODUCT
  error("Head `$head` does not have a corresponding scalar expression.")
end

primitive type ID 64 end

ID(val::UInt64) = reinterpret(ID, val)
ID(val::Integer) = ID(UInt64(val))

mutable struct IDCounter
  val::UInt64
end

next!(counter::IDCounter) = ID(counter.val += 1)
IDCounter() = IDCounter(0)

struct ExpressionSpec
  head::Head
  args::Vector{Any}
end

Base.:(==)(x::ExpressionSpec, y::ExpressionSpec) = x.head == y.head && x.args == y.args
Base.hash(spec::ExpressionSpec, h::UInt) = hash(hash(spec.head) + hash(spec.args), h)

ExpressionSpec(head::Head, args::AbstractVector) = ExpressionSpec(head, convert(Vector{Any}, args))
ExpressionSpec(head::Head, args...) = ExpressionSpec(head, collect(Any, args))


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
Base.hash(ex::Expression, h::UInt) = h ⊻ hash(Expression, hash(ex.head) + hash(ex.grade) + hash(ex.args))

const Term = Union{Expression,ID}

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
  substitutions::Dict{ExpressionSpec,Union{Object,Expression}}
end

Base.broadcastable(cache::ExpressionCache) = Ref(cache)

ExpressionCache(sig::Signature) = ExpressionCache(sig, IDCounter(), IdDict(), IdDict(), Dict())

is_expression_caching_enabled() = true

Base.getproperty(ex::Expression, field::Symbol) = field === :cache ? getfield(ex, field)::ExpressionCache : getfield(ex, field)

Expression(head::Head, ex::Expression) = Expression(ex.cache, head, ex)
Expression(cache::ExpressionCache, head::Head, args...) = Expression(cache, ExpressionSpec(head, args...))
function Expression(cache::ExpressionCache, spec::ExpressionSpec)
  existing = get(cache.substitutions, spec, nothing)
  !isnothing(existing) && is_expression_caching_enabled() && return existing
  build_expression!(cache, spec)
end

function build_expression!(cache::ExpressionCache, spec::ExpressionSpec)
  (; head, args) = spec
  for (i, arg) in enumerate(args)
    isa(arg, Expression) && continue
    isa(arg, ID) && continue
    id = get!(() -> next!(cache.counter), cache.primitive_ids, Object(arg))
    args[i] = id
    cache.primitives[id] = Object(arg)
  end
  ex = Expression(head, args, cache)
  is_expression_caching_enabled() && (cache.substitutions[spec] = ex)
  ex
end

dereference(cache::ExpressionCache, primitive_id::ID) = cache.primitives[primitive_id].val
dereference(cache::ExpressionCache, x) = x
function dereference(ex::Expression)
  @assert length(ex) == 1
  dereference(ex.cache, ex[1]::ID)
end

substitute!(ex::Expression, head::Head, args...) = substitute!(ex, ExpressionSpec(head, args...))
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

  # Turn non-scalar expressions into scalar expressions if all elements are in fact scalars.
  if head in (ADDITION, SUBTRACTION, NEGATION, INVERSE, EXPONENTIAL) && all(isscalar, args)
    return substitute!(ex, promote_scalar(head), args)
  end

  if head === SCALAR
    @assert length(args) == 1
    isa(args[1], Expression) && return substitute!(ex, args[1])
  end

  if in(head, (GEOMETRIC_PRODUCT, ADDITION, SCALAR_PRODUCT, SCALAR_ADDITION))
    length(args) == 1 && return substitute!(ex, args[1]::Expression)

    # Disassociate products and additions.
    any(isexpr(head), args) && return substitute!(ex, disassociate1(args, head))
  end


  if head in (ADDITION, SCALAR_ADDITION)
    new_args = factorize_addition_terms(cache, args)
    isempty(new_args) && return scalar(cache, 0)
    new_args !== args && return substitute!(ex, head, new_args)
  end

  if head in (SCALAR_PRODUCT, SCALAR_ADDITION)
    new_args = propagate_constants(cache, head, args)
    args !== new_args && return substitute!(ex, head, new_args)
  end

  # Remove unit elements for multiplication and addition.
  head in (GEOMETRIC_PRODUCT, SCALAR_PRODUCT, ADDITION, SCALAR_ADDITION) && remove_unit_elements!(args, head) && return substitute!(ex, head, args)

  # Apply left and right complements.
  if head in (LEFT_COMPLEMENT, RIGHT_COMPLEMENT)
    isweightedblade(args[1]) && return substitute!(ex, weighted(Expression(cache, head, args[1][2]), args[1][1]))
    args[1] == scalar(cache, 0) && return args[1]
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

  if head in (SUBTRACTION, SCALAR_SUBTRACTION)
    # Simplify unary minus operator to a multiplication with -1.
    length(args) == 1 && return substitute!(ex, weighted(args[1], -1))
    # Transform binary minus expression into an addition.
    @assert length(args) == 2
    return substitute!(ex, ADDITION, args[1], -args[2])
  end

  if head === GEOMETRIC_PRODUCT
    # Collapse scalars into one and put it at the front.
    nf = count(isscalar, args)
    if nf ≥ 2
      # Collapse all scalars into one at the front.
      scalars, nonscalars = filter(isscalar, args), filter(!isscalar, args)
      fac = Expression(cache, SCALAR_PRODUCT, scalars)
      length(args) == nf && return substitute!(ex, fac)
      return substitute!(ex, GEOMETRIC_PRODUCT, Any[fac; nonscalars])
    elseif nf == 1 && !isscalar(args[1])
      # Put the factor at the front.
      i = findfirst(isscalar, args)
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
      return substitute!(ex, GEOMETRIC_PRODUCT, Any[args; blade_ex])
    end
  end

  # Check that arguments make sense.
  n = expected_nargs(head)
  !isnothing(n) && @assert length(args) == n "Expected $n arguments for expression $head, $(length(args)) were provided\nArguments: $args"
  @assert !isempty(args) || head === BLADE
  head === BLADE && @assert all(isa(dereference(cache, i), Int) for i in args) "Expected integer arguments in BLADE expression, got $(dereference.(cache, ex))"

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
    sc = Expression(cache, GEOMETRIC_PRODUCT, Expression(cache, REVERSE, a), a)
    isscalar(sc) || error("`reverse(A) ⟑ A` is not a scalar, suggesting that A is not a versor. Inversion is only supported for versors at the moment.")
    return substitute!(ex, GEOMETRIC_PRODUCT, Expression(cache, REVERSE, a), Expression(cache, INVERSE, sc))
  end

  ex.grade = infer_grade(cache, head, args)::GradeInfo

  ex
end

"""
Group addition terms by introspecting at integer factors for directly nested products and grouping terms based on these factors.
If a factor is zero, the term is omitted in the returned arguments.
"""
function factorize_addition_terms(cache, terms)
  counter = 0
  ids = Dict{Any,Int}()
  id_counts = Dict{Int,Int}()
  new_terms = Term[]
  for term in terms
    n = 1
    if isexpr(term, (SCALAR_PRODUCT, GEOMETRIC_PRODUCT))
      @assert isscalar(term[1])
      all_terms = isexpr(term, SCALAR_PRODUCT) ? term.args : [term[1].args; term[2:end]]
      if isexpr(all_terms[1], SCALAR)
        weight = dereference(all_terms[1])
        if isa(weight, Int) || isa(weight, Integer)
          n = weight
          term = if isexpr(term, SCALAR_PRODUCT)
            length(term) == 2 ? term[2] : Expression(cache, SCALAR_PRODUCT, term[2:end])
          else
            scalar_part = term[1]
            scalar_part = length(scalar_part) == 2 ? scalar_part[2] : Expression(cache, SCALAR_PRODUCT, scalar_part[2:end])
            Expression(cache, GEOMETRIC_PRODUCT, Term[scalar_part; term[2:end]])
          end
        end
      end
    end
    id = get!(() -> (counter += 1), ids, term)
    id_counts[id] = something(get(id_counts, id, nothing), 0) + n
  end
  for (term, id) in sort(collect(ids); by = last)
    n = id_counts[id]
    n == 0 && continue
    new_term = n == 1 ? term : Expression(cache, GEOMETRIC_PRODUCT, scalar(cache, n), term)
    push!(new_terms, new_term)
  end
  new_terms == terms && return terms
  new_terms
end

function propagate_constants(cache, head, args)
  fac = head == SCALAR_ADDITION ? 0 : 1
  final_args = []
  for arg in args
    if isexpr(arg, SCALAR)
      x = dereference(arg)
      if isa(x, Int) || isa(x, Integer)
        head == SCALAR_ADDITION ? (fac += x) : (fac *= x)
        continue
      end
    end
    push!(final_args, arg)
  end
  if fac == 0 && head == SCALAR_PRODUCT
    any(!isscalar, args) && @debug "Non-scalar expression annihilated by a zero multiplication term" args
    return Term[]
  end
  if length(args) > length(final_args) + 1
    sc = scalar(cache, fac)
    !isunit(sc) && pushfirst!(final_args, sc)
    return final_args
  end
  args
end

isunit(sc::Expression) = isexpr(sc, SCALAR_PRODUCT) ? isone(dereference(sc)) : isexpr(sc, SCALAR_ADDITION) ? iszero(dereference(fac)) : @assert false

isscalar(ex::Expression, x::Number) = isexpr(ex, SCALAR) && dereference(ex) == x

function remove_unit_elements!(args, head)
  n = length(args)
  if head in (GEOMETRIC_PRODUCT, SCALAR_PRODUCT)
    filter!(x -> !isscalar(x, 1), args)
  elseif head in (ADDITION, SCALAR_ADDITION)
    filter!(x -> !isscalar(x, 0), args)
  end
  did_remove = n > length(args)
  did_remove
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

# function simplify_negations(ex::Expression)
#   negated = count(x -> isscalar(x, -1), ex)
#   negated in (0, 1) && return ex
#   args = filter(x -> !isscalar(x, -1), ex.args)
#   isodd(negated) && pushfirst!(args, scalar(ex.cache, -1))
#   isempty(args) && return scalar(ex.cache, 1)
#   Expression(ex, SCALAR_PRODUCT, args)
# end

function expected_nargs(head)
  in(head, (SCALAR, NEGATION, REVERSE, ANTIREVERSE, LEFT_COMPLEMENT, RIGHT_COMPLEMENT, INVERSE, EXPONENTIAL, SCALAR_NEGATION, SCALAR_INVERSE, SCALAR_EXPONENTIAL)) && return 1
  in(head, (INTERIOR_PRODUCT, COMMUTATOR_PRODUCT, SUBTRACTION)) && return 2
  nothing
end

function infer_grade(cache, head::Head, args)
  isscalar(head) && return 0
  head === BLADE && return count(x -> isodd(dereference(cache, x)), map(x -> count(==(x), args), unique(args)))
  if head === GEOMETRIC_PRODUCT
    @assert length(args) == 2 && isscalar(args[1])
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
  blade(cache, reverse!(setdiff(1:dimension(sig), basis_vectors(b)))) * scalar(cache, (-1)^(
    isodd(sum(basis_vectors(b); init = 0)) +
    (dimension(sig) ÷ 2) % 2 +
    isodd(dimension(sig)) & isodd(length(b))
  ))
end

# Exact formula derived from the right complement.
blade_left_complement(b::Expression) = scalar(b.cache, (-1)^(grade(b) * antigrade(b))) * blade_right_complement(b)

function project!(ex, g, level = 0)
  isa(ex, Expression) || return ex
  if all(isempty(intersect(g, g′)) for g′ in grade(ex))
    iszero(level) && @debug "Non-scalar expression annihilated in projection into grade(s) $g" ex
    return scalar(ex.cache, 0)
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
  res = Any[]
  for arg in ex
    arg::Expression
    skip = reverse_op === REVERSE ? in(arg.grade, (0, 1)) : in(antigrade(cache.sig, arg.grade), (0, 1))
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
  α = Expression(cache, SCALAR_SQRT, Expression(cache, SCALAR_ABS, α²))
  # The sign may not be deducible from α² directly, so we compute it manually given metric simplifications and blade permutations.
  is_negative = isodd(count(metric(sig, v) == -1 for v in vs)) ⊻ iseven(length(vs))
  # Negative square: cos(α) + A * sin(α) / α
  # Positive square: cosh(α) + A * sinh(α) / α
  (s, c) = is_negative ? (SCALAR_SIN, SCALAR_COS) : (SCALAR_SINH, SCALAR_COSH)
  Expression(cache, ADDITION, scalar(cache, Expression(cache, c, α)), Expression(cache, GEOMETRIC_PRODUCT, scalar(cache, Expression(cache, SCALAR_NAN_TO_ZERO, Expression(cache, SCALAR_DIVIDE, Expression(cache, s, α), α))), b))
end

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
isscalar(ex::Expression) = isblade(ex, 0) || isweightedblade(ex, 0) || isscalar(ex.head)
isweighted(ex) = isexpr(ex, GEOMETRIC_PRODUCT, 2) && isscalar(ex[1]::Expression)
isweightedblade(ex, n = nothing) = isweighted(ex) && isblade(ex[2]::Expression, n)

function basis_vectors(ex::Expression)
  isweightedblade(ex) && return basis_vectors(ex[2]::Expression)
  isexpr(ex, BLADE) && return [dereference(ex.cache, i)::Int for i in ex]
  error("Expected blade or weighted blade expression, got $ex")
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

weighted(x::Expression, fac) = Expression(x.cache, GEOMETRIC_PRODUCT, scalar(x.cache, fac), x)
scalar(cache::ExpressionCache, fac) = Expression(cache, SCALAR, fac)
blade(cache::ExpressionCache, xs...) = Expression(cache, BLADE, xs...)
kvector(args::AbstractVector) = Expression((args[1]::Expression).cache, KVECTOR, args)
kvector(xs...) = kvector(collect(Any, xs))
multivector(args::AbstractVector) = Expression((args[1]::Expression).cache, MULTIVECTOR, args)
multivector(xs...) = multivector(collect(Any, xs))
Base.reverse(ex::Expression) = Expression(REVERSE, ex)
antireverse(ex::Expression) = Expression(ANTIREVERSE, ex)
antiscalar(cache::ExpressionCache, fac) = scalar(cache, fac) * antiscalar(cache)
antiscalar(cache::ExpressionCache) = blade(cache, 1:dimension(cache.sig))

⟑(x::Expression, args::Expression...) = Expression(x.cache, GEOMETRIC_PRODUCT, x, args...)
Base.:(*)(x::Expression, args::Expression...) = Expression(x.cache, GEOMETRIC_PRODUCT, x, args...)
Base.:(*)(x::Number, y::Expression, args::Expression...) = *(scalar(y.cache, x), y, args...)
Base.:(+)(x::Expression, args::Expression...) = Expression(x.cache, ADDITION, x, args...)
Base.:(-)(x::Expression, args::Expression...) = Expression(x.cache, SUBTRACTION, x, args...)
∧(x::Expression, y::Expression) = Expression(x.cache, EXTERIOR_PRODUCT, x, y)
exterior_product(x::Expression, y::Expression) = Expression(x.cache, EXTERIOR_PRODUCT, x, y)

# Traversal/transformation utilities.

walk(ex::Expression, inner, outer) = outer(Expression(ex.cache, ex.head, filter!(!isnothing, inner.(ex.args))))
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
  isscalar(ex) && return print_scalar(io, ex)
  if isexpr(ex, GEOMETRIC_PRODUCT) && isexpr(ex[end], BLADE)
    if length(ex) == 2 && isexpr(ex[1], SCALAR) || isexpr(ex[1], SCALAR_COMPONENT)
      return print(io, ex[1], " ⟑ ", ex[end])
    else
      return print(io, '(', join(ex[1:(end - 1)], " ⟑ "), ") ⟑ ", ex[end])
    end
  end
  isexpr(ex, (GEOMETRIC_PRODUCT, ADDITION, MULTIVECTOR)) && return print(io, '(', Expr(:call, head_symbol(ex.head), ex.args...), ')')
  isexpr(ex, KVECTOR) && return print(io, Expr(:call, Symbol("kvector", subscript(ex.grade)), ex.args...))
  print_expression(io, ex)
end

function head_symbol(head::Head)
  isscalar(head) && return nameof(scalar_function(head))
  head === GEOMETRIC_PRODUCT && return :⟑
  head === ADDITION && return :+
  head === MULTIVECTOR && return :multivector
end

print_expression(io, ex::Expression) = print(io, Expression, "(:", ex.head, ", ", join(ex.args, ", "), ')')

isinfix(head) = head in (SCALAR_ADDITION, SCALAR_PRODUCT)

function print_scalar(io, ex::Expression)
  if isinfix(ex.head)
    repr = join((sprint(print_scalar, arg) for arg in ex), " $(head_symbol(ex.head)) ")
    return isexpr(ex, SCALAR_ADDITION) ? print(io, '(', repr, ')') : print(io, repr)
  end
  isexpr(ex, SCALAR_COMPONENT, 1) && return print(io, isa(ex[1], Number) ? ex[1] : Expr(:ref, ex[1]))
  isexpr(ex, SCALAR_COMPONENT) && return print(io, Expr(:ref, ex...))
  isexpr(ex, SCALAR) && return print(io, dereference(ex))
  print_expression(io, ex)
end

"""
Return `val` as a subscript, used for printing `UnitBlade` and `Blade` instances.
"""
function subscript(val)
  r = div(val, 10)
  subscript_char(x) = Char(8320 + x)
  r > 0 ? string(subscript_char(r), subscript_char(mod(val, 10))) : string(subscript_char(val))
end
