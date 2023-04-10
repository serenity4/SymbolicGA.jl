struct Factorization
  ex::Expression
  terms::Vector{Term}
  # Term -> list of products in which the term appears.
  factors::Dictionary{Term, Vector{Term}}
end

function Factorization(ex::Expression)
  @assert isexpr(ex, SCALAR_ADDITION)
  terms = ex.args
  Factorization(ex, terms, factor_map(terms))
end

function factor_map(products)
  factors = Dictionary{Term, Vector{Term}}()
  for product in products
    isexpr(product, SCALAR_PRODUCT) || continue
    for term in product
      term_products = get!(Vector{Term}, factors, term)
      !in(product, term_products) && push!(term_products, product)
    end
  end
  factors
end

function remove_single_factor(term::Term, factor::Term)
  @assert term !== factor
  isexpr(term, SCALAR_PRODUCT) || return term
  ex = term::Expression
  removed = findfirst(term -> term === factor, ex)
  @assert !isnothing(removed)
  length(ex) == 2 && return removed == 1 ? ex[2] : ex[1]
  unsimplified_expression(ex.cache, SCALAR_PRODUCT, Term[ex[i] for i in eachindex(ex) if i ≠ removed])
end

function factorize(ex::Expression)
  !isexpr(ex, SCALAR_ADDITION) && return ex
  fact = Factorization(ex)
  apply!(fact)
end

function apply!(fact::Factorization)
  (; terms, factors, ex) = fact
  (; cache) = ex
  # Not enough products to factorize anything.
  length(factors) < 2 && return ex
  n, factor = findmax(length, factors)
  @assert n > 0
  # There are no terms that we can factorize.
  n == 1 && return ex
  @debug "Factorizing $n terms for $ex"
  products = factors[factor]
  factorized_terms = Term[remove_single_factor(term, factor) for term in products]
  @assert any(xx !== yy for (xx, yy) in zip(factorized_terms, products))
  @assert length(factorized_terms) > 1
  addition = unsimplified_expression(cache, SCALAR_ADDITION, factorized_terms)
  product = unsimplified_expression(cache, SCALAR_PRODUCT, disassociate1(Term[factor, factorize(addition)], SCALAR_PRODUCT))
  unfactorized_terms = filter(term -> all(x -> x !== term, products), terms)
  isempty(unfactorized_terms) && return product
  unfactorized = length(unfactorized_terms) == 1 ? unfactorized_terms[1] : factorize(unsimplified_expression(cache, SCALAR_ADDITION, unfactorized_terms))
  new_ex = unsimplified_expression(cache, SCALAR_ADDITION, disassociate1(Term[product, unfactorized], SCALAR_ADDITION))
  factorize(new_ex)
end

function replace_with_factorized!(cache, ex, i)
  isnothing(i) && return true
  iterator = traversal_iterator(ex)
  subex = iterator[i]
  isa(subex, ID) && (subex = dereference(cache, subex))
  !isexpr(subex, (SCALAR_ADDITION, SCALAR_PRODUCT)) && return true
  factorized = factorize(subex)
  iterator[i] = factorized
  for term in leaf_factorization_terms(factorized)
    factorize!(term, cache)
  end
  false
end

function leaf_factorization_terms(ex::Expression)
  leaves = Term[]
  traverse(ex) do ex
    isexpr(ex, (SCALAR_ADDITION, SCALAR_PRODUCT)) && return true
    push!(leaves, ex)
    false
  end
  leaves
end

factorize!(ex::Expression) = factorize!(ex, ex.cache)

function factorize!(ex, cache::ExpressionCache)
  traverse_indexed((ex, i) -> replace_with_factorized!(cache, ex, i), ex; retraversal = Retraversal(cache, Expr))
  ex
end
