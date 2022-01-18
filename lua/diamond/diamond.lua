local A = ("A"):byte()

local diamond = function (char)
    local size = char:byte() - A
    local lines = {}
    for i = 0, size do
        local ch = string.char(A + i)
        local half = (" "):rep(i) .. ch .. (" "):rep(size-i)
        lines[#lines+1] = half:sub(2):reverse() .. half .. "\n"
    end
    for i = size, 1, -1 do
        lines[#lines+1] = lines[i]
    end
    return table.concat(lines)
end
 
return diamond
