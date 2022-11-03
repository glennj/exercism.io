-- Iteratively,
local function find(array, search_for)
    local i = 1
    local j = #array

    while i <= j do
        local mid = (i + j) // 2
        local val = array[mid]

        if search_for == val then
            return mid
        elseif search_for < val then
            j = mid - 1
        else
            i = mid + 1
        end
    end

    return -1       -- not found
end

-- Recursively,
local function find_rec(array, search_for)
    local function searcher(i, j)
        if i > j then
            return -1
        end

        local mid = (i + j) // 2
        local val = array[mid]

        if search_for == val then
            return mid
        elseif search_for < val then
            return searcher(i, mid - 1)
        else
            return searcher(mid + 1, j)
        end
    end

    return searcher(1, #array)
end

return find_rec
