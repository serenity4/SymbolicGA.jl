function expand_operators(ex::Expression)
  prewalk(ex) do ex
    if isexpr(ex, :∧)
      # The outer product is associative, no issues there.
      project(sum(grade, ex), Expression(:*, ex.args))
    elseif isexpr(ex, :⋅)
      length(ex) == 2 || error("The inner product must have only two operands, as no associativity law is available to derive a canonical binarization.")
      # Homogeneous vectors are expected, so the grade should be known.
      r, s = grade(ex[1])::Int, grade(ex[2])::Int
      project(iszero(r) || iszero(s) ? nothing : abs(r - s), Expression(:*, ex.args))
    elseif isexpr(ex, :⦿)
      project(0, Expression(:*, ex.args))
    else
      ex
    end
  end
end

function distribute(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, (:kvector, :multivector)) && return Expression(:+, ex.args)
    isexpr(ex, :+) && return disassociate1(ex, :+)
    isexpr(ex, :*) && any(isexpr(arg, :+) for arg in ex.args) && return distribute1(ex)
    ex
  end
end

function disassociate1(args, op::Symbol)
  new_args = []
  for arg in args
    if isexpr(arg, op)
      append!(new_args, arg.args)
    else
      push!(new_args, arg)
    end
  end
  new_args
end

disassociate1(ex::Expression, op::Symbol) = Expression(op, disassociate1(ex.args, op))

function distribute1(ex::Expression)
  x, ys = ex[1], @view ex[2:end]
  base = isexpr(x, :+) ? x.args : [x]
  for y in ys
    new_base = []
    yterms = isexpr(y, :+) ? y.args : (y,)
    for xterm in base
      for yterm in yterms
        push!(new_base, xterm * yterm)
      end
    end
    base = new_base
  end
  Expression(:+, base)
end

"""
Convert sums between elements of arbitrary grades into either a k-vector or a multivector.

If all elements in the sum had the same grade, a k-vector is returned.
Otherwise, all elements of the same grade are grouped, wrapped in k-vectors
and added to a multivector expression.
"""
function restructure_sums(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, :+) || return ex

    if count(==(ex[1].grade::Int), arg.grade::Int for arg in ex) == length(ex)
      return kvector(ex.args)
    end

    args = sort(ex.args, by = x -> x.grade::Int)
    grades = getproperty.(args, :grade)
    i = 1
    new_args = []
    while i ≤ lastindex(grades)
      g = grades[i]::Int
      j = findfirst(≠(g), @view grades[(i + 1):end])
      j = something(j, lastindex(grades) - (i - 1))
      j += i
      if j > i + 1
        push!(new_args, kvector(args[i:(j - 1)]))
      else
        push!(new_args, args[i])
      end
      i = j
    end
    multivector(new_args)
  end
end

function apply_projections(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, :project) || return ex
    g, ex = ex.args
    if isexpr(ex, :multivector)
      kvector(filter(x -> grade(x)::Int == g, ex.args))
    else
      Expression(ex.head, filter(x -> grade(x)::Int == g, ex.args))
    end
  end
end

function canonicalize_blades(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, :blade) || return ex
    perm = sortperm(ex.args, by = x -> x[1]::Int)
    new_ex = blade(ex[perm])
    iseven(parity(perm)) && return new_ex
    weighted(new_ex, -1)
  end
end

function apply_metric(ex::Expression, s::Signature)
  postwalk(ex) do ex
    isexpr(ex, :blade) || return ex
    @assert all(isexpr(arg, :basis) for arg in ex.args)
    new_args = copy(ex.args)
    last = nothing
    fac = 1
    i = 1
    while length(new_args) ≥ 2
      i > lastindex(new_args) && break
      arg = new_args[i]
      new = arg[1]::Int
      if !isnothing(last)
        if new == last
          m = metric(s, last)
          iszero(m) && return nothing
          fac *= m
          length(new_args) == 2 && return scalar(fac)
          deleteat!(new_args, i)
          deleteat!(new_args, i - 1)
          i = max(i - 2, 1)
          last = i < firstindex(new_args) ? nothing : new_args[i][1]::Int
        else
          last = new
        end
      else
        last = new
      end
      i += 1
    end
    if isempty(new_args)
      scalar(fac)
    else
      new_ex = blade(new_args)
      isone(fac) ? new_ex : weighted(new_ex, fac)
    end
  end
end

function disassociate_kvectors(ex::Expression)
  postwalk(ex) do ex
    isexpr(ex, :kvector) && any(isexpr(:kvector), ex.args) && return disassociate1(ex, :kvector)
    ex
  end
end

function group_kvector_blades(ex::Expression)
  isexpr(ex, :multivector) && return Expression(:multivector, group_kvector_blades.(ex.args))
  isexpr(ex, :scalar) && return ex
  isexpr(ex, :blade) && return ex
  isweighted(ex) && isexpr(ex[2], :blade) && return ex
  @assert isexpr(ex, :kvector) "Expected k-vector expression, got $ex"
  grade(ex) == 0 && return kvector(Expression(:+, ex.args))

  blade_weights = Dict{Vector{Int},Expression}()
  for arg in ex.args
    if isweighted(arg)
      weight, blade = arg[1]::Expression, arg[2]::Expression
    else
      weight, blade = Expression(:scalar, 1), arg
    end
    indices = basis_vectors(arg)
    if haskey(blade_weights, indices)
      blade_weights[indices] = blade_weights[indices] + weight
    else
      blade_weights[indices] = weight
    end
  end
  kvector(Any[weighted(blade(indices), weight) for (indices, weight) in pairs(blade_weights)])
end

function basis_vectors(ex::Expression)
  isexpr(ex, :*, 2) && isexpr(ex[1], :scalar) && isexpr(ex[2], :blade) && return basis_vectors(ex[2]::Expression)
  isexpr(ex, :blade) && return Int[arg[1] for arg in ex]
  isexpr(ex, :scalar) && return Int[]
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

function fill_kvector_components(ex::Expression, s::S) where {S<:Signature}
  postwalk(ex) do ex
    isexpr(ex, :kvector) || return ex
    g = grade(ex)::Int
    i = 1
    ex = kvector(sort(ex.args, by = basis_vectors, lt = lt_basis_order))
    for indices in combinations(1:dimension(S), g)
      next = i ≤ lastindex(ex) ? ex[i]::Expression : nothing
      if isnothing(next) || indices ≠ basis_vectors(next)
        insert!(ex.args, i, weighted(blade(indices), 0))
      end
      i += 1
    end
    ex
  end
end
