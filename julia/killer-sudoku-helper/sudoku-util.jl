function combinations_in_cage(sum, size)
    combinations_in_cage(sum, size, [])
end

function combinations_in_cage(sum, size, excludes)
    if size == 1 
        if 1 ≤ sum ≤ 9 && sum ∉ excludes
            return [[sum]]
        end
        return []
    end

    results = []
    for n ∈ 1:9
        n ∈ excludes && continue

        for comb ∈ combinations_in_cage(sum - n, size - 1, vcat(excludes, [n]))
            push!(results, sort(vcat(comb, [n])))
        end
    end
    unique(results)
end
