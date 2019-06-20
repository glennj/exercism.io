function binarysearch(
    haystack::AbstractArray,
    needle,
    i::Integer=1,
    j::Integer=length(haystack);
    rev = false,
    lt  = <,
    by  = identity
)::UnitRange{Integer}

    rev && (lt = >)

    mid = i + Int(ceil((j - i) / 2))

    if i > j
        # element not in haystack
        i:j

    elseif by(needle) == by(haystack[mid])
        # found it
        mid:mid

    else
        if lt(by(needle), by(haystack[mid]))
            j = mid - 1
        else
            i = mid + 1
        end

        binarysearch(haystack, needle, i, j, rev = rev, lt = lt, by = by)
    end
end
