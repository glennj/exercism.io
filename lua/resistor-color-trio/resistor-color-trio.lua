local code = {
  black = 0, brown = 1, red    = 2, orange = 3, yellow = 4,
  green = 5, blue  = 6, violet = 7, grey   = 8, white  = 9,
}

local prefix = { "", "kilo", "mega", "giga" }


function decode(c1, c2, c3)
  local value, idx
  value = resistor_value(c1, c2, c3)
  value, idx = magnitude(value)
  return value, prefix[idx].."ohms"
end


function resistor_value(c1, c2, c3)
  return (10 * code[c1] + code[c2]) * 10 ^ code[c3]
end


function magnitude(value)
  local idx = 1
  while value == value // 1000 * 1000 do
    value = value // 1000
    idx = idx + 1
  end
  return value, idx
end


return {
  decode = decode
}
