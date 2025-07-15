local hamming = {}

function hamming.compute(first, second)
    assert(#first == #second, "strands must be of equal length")

    local diff = 0
    for i = 1, #first do
        if first:sub(i, i) ~= second:sub(i, i) then
            diff = diff + 1
        end
    end
    return diff
end

return hamming
