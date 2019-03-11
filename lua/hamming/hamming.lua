local hamming = {}

function hamming.compute(first, second)
    if #first ~= #second then
        return -1
    end

    local diff = 0
    for i = 1, #first do
        --if string.sub(first, i, i) ~= string.sub(second, i, i) then
        if first:sub(i, i) ~= second:sub(i, i) then
            diff = diff + 1
        end
    end
    return diff
end

return hamming
