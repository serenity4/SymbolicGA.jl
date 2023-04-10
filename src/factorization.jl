struct Factorization
  ex::Expression
  # Term -> list of products in which the term appears.
  factors::Dict{Term, Vector{Term}}
  # Term -> list of products in which the term appears more than once.
  groups::Dict{Term, Vector{Term}}
end

function Factorization(ex::Expression)
  products = isexpr(ex, SCALAR_ADDITION) ? ex.args : Term[ex]
  Factorization(ex, factor_map(products), factorization_groups(products))
end

function factor_map(products)
  factors = Dict{Term, Vector{Term}}()
  for product in products
    length(product) == 1 && continue
    for term in product
      push!(get!(Vector{Term}, factors, term), product)
    end
  end
  factors
end

function factorization_groups(products)
  groups = Dict{Term,Vector{Term}}()
  for product in products
    length(product) == 1 && continue
    for term in product
      n = count(x -> in(term, x), products)
      n == 1 && continue
      push!(get!(Vector{Term}, groups, term), product)
    end
  end
  groups
end

function remove_single_factor(term::Term, factor::Term)
  @assert term !== factor
  length(term) == 1 && return term
  ex = term::Expression
  length(ex) == 2 && return ex[1] === factor ? ex[2] : ex[1]
  unsimplified_expression(ex.cache, ex.head, Term[term for term in ex if term !== factor])
end

function factorize(ex::Expression)
  fact = Factorization(ex)
  apply!(fact)
end

function apply!(fact::Factorization)
  (; factors, ex) = fact
  (; cache) = ex
  terms = isexpr(ex, SCALAR_ADDITION) ? ex.args : Term[ex]
  length(factors) < 2 && return fact.ex
  pairs = collect(factors)
  n, i = findmax(length ∘ last, pairs)
  @assert n > 0
  n == 1 && return ex
  @debug "Factorizing $n terms"
  factor, factorized_terms = pairs[i]
  factorized = Term[remove_single_factor(term, factor) for term in factorized_terms]
  sum = length(factorized) == 1 ? factorized[1] : unsimplified_expression(cache, SCALAR_ADDITION, factorized)
  product = unsimplified_expression(cache, SCALAR_PRODUCT, factor, factorize(sum))
  unfactorized_terms = filter(!in(factorized_terms), terms)
  isempty(unfactorized_terms) && return product
  unfactorized = length(unfactorized_terms) == 1 ? unfactorized_terms[1] : factorize(unsimplified_expression(cache, SCALAR_ADDITION, unfactorized_terms))
  unsimplified_expression(cache, SCALAR_ADDITION, product, unfactorized)
end
