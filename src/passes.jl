"""
Convert sums between elements of arbitrary grades into either a k-vector or a multivector.

If all elements in the sum had the same grade, a k-vector is returned.
Otherwise, all elements of the same grade are grouped, wrapped in k-vectors
and added to a multivector expression.
"""
function restructure_sums(ex::Expression, sig::Signature)
  ex == factor(0) && return kvector(scalar(Zero()))
  if isblade(ex) || isweightedblade(ex)
    isone(nelements(sig, ex.grade::Int)) && return kvector(ex)
    terms = [ex]
  else
    @assert isexpr(ex, :+) "Expected addition expression, got expression type $(ex.head)"
    terms = ex.args
  end

  # Fast path when all terms have the same grade.
  allequal(grade.(terms)) && return kvector(terms)

  args = sort(terms, by = grade)
  grades = grade.(args)
  i = 1
  new_args = []
  while i ≤ lastindex(grades)
    g = grades[i]
    j = findfirst(≠(g), @view grades[(i + 1):end])
    j = something(j, lastindex(grades) - (i - 1))
    j += i
    push!(new_args, kvector(args[i:(j - 1)]))
    i = j
  end
  multivector(new_args)
end

"Non-simplifiable zero, for use in factors."
struct Zero end
Base.show(io::IO, ::Zero) = print(io, '𝟎')

function fill_kvector_components(ex::Expression, s::Signature)
  postwalk(ex) do ex
    isexpr(ex, :kvector) || return ex
    g = grade(ex)
    i = 1
    ex = kvector(sort(ex.args, by = basis_vectors, lt = lt_basis_order))
    for indices in combinations(1:dimension(s), g)
      next = i ≤ lastindex(ex) ? ex[i]::Expression : nothing
      if isnothing(next) || indices ≠ basis_vectors(next)
        insert!(ex.args, i, weighted(blade(indices), Zero()))
      end
      i += 1
    end
    ex
  end
end
