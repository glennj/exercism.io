local function hamming(first, second)
    if #first ~= #second then
        return -1
    end

    local diff = 0
    for i = 1, #first do
        if first:sub(i, i) ~= second:sub(i, i) then
            diff = diff + 1
        end
    end
    return diff
end

return {
    compute = hamming
}
