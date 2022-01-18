local PhoneNumber = {}

local nanp_pattern = '^1?(' .. string.rep('%d', 10) .. ')$'
local default_number = string.rep('0', 10)

-- However, a real NANP number prevents the area code or exchange 
-- from starting with 0 or 1:
-- local nanp_pattern = '^1?([2-9]%d%d)([2-9]%d%d)(%d%d%d%d)$'

function PhoneNumber:new(string)
    local phone = {}
    setmetatable(phone, self)
    self.__index = self

    string = (string or ""):gsub('%D', '')
    phone.number = string:match(nanp_pattern) or default_number
    phone._parts = { phone.number:match('(...)(...)(....)') }
    phone._str   = ('(%s) %s-%s'):format(table.unpack(phone._parts))

    return phone
end

function PhoneNumber:areaCode  () return self._parts[1] end
function PhoneNumber:__tostring() return self._str      end

return PhoneNumber
