local split = require("split")  -- https://luarocks.org/modules/telemachus/split

local trim_result
local transpose_matrix
local equalize_length
local map
local rtrim
local rpad

local transpose = function (input)
    if input == '' then return '' end
    local transposed = 
        trim_result(
            transpose_matrix(
                equalize_length(
                    split.split(input, "\n")
                )
            )
        )
    return table.concat(transposed, "\n")
end

-- remove trailing whitespace so that, from the bottom up,
-- each line length is <= the line above
trim_result = function (lines)
    local i, len = #lines, -1
    repeat
        local trimmed = rtrim(lines[i])
        if #trimmed < len then
            trimmed = rpad(trimmed, len)
        else
            len = #trimmed
        end
        lines[i] = trimmed
        i = i - 1
    until i == 0
    return lines
end

transpose_matrix = function (lines)
    local result = {}
    for c = 1, #lines[1] do
        result[c] = ""
        for r = 1, #lines do
            result[c] = result[c] .. lines[r]:sub(c,c)
        end
    end
    return result
end

equalize_length = function (lines)
    local line_lengths = map(lines, function(line) return #line end)
    local maxlen = math.max(table.unpack(line_lengths))
    return map(lines, function(line) return rpad(line, maxlen) end)
end

map = function (list, func)
    local result = {}
    for i, v in ipairs(list) do
        result[i] = func(v)
    end
    return result
end

rpad = function (s, len, fill)
    return s .. (fill or " "):rep(len - #s)
end

rtrim = function (s, charset)
    return s:gsub((charset or '%s').."*$", "")
end

return transpose
