local rc = require 'resistor-color'
local rcd = require 'resistor-color-duo'

local prefix = { "", "kilo", "mega", "giga" }


function decode(c1, c2, c3)
  local val = rcd.value({c1, c2}) * 10 ^ rc.color_code(c3)
  local idx = 1

  while val > 0 and val % 1000 == 0 do
    val = val / 1000
    idx = idx + 1
  end

  return val, prefix[idx].."ohms"
end


return {
  decode = decode
}
