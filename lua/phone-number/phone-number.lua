local nanp_pattern = '^1?([2-9]%d%d)([2-9]%d%d)(%d%d%d%d)$'
local valid_nondigits = '[ +().-]'

local function clean(string)
    local cleaned = string:gsub(valid_nondigits, '')
    local areaCode, exchange, number = cleaned:match(nanp_pattern)
    assert(areaCode, 'invalid phone number')
    return areaCode .. exchange .. number
end

return { clean = clean }
