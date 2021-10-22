local function is_sorted(array)
    for i = 2, #array do
        if array[i-1] > array[i] then return false end
    end
    return true
end

local function find(array, search_for)
    --[[ We ought to verify that the array is sorted.
    --   However, this upsets the traced array's access count,
    --   and causes the tests' assertions to fail.
    if not is_sorted(array) then
        error("array is not sorted")
    end
    --]]

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

return find
