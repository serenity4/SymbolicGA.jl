linear_index(combinations, grade::Integer) = findfirst(==(grade), length.(combinations))
linear_index(combinations, indices) = findfirst(==(indices), combinations)

indices_from_linear_index(combinations, index::Integer) = combinations[index]
