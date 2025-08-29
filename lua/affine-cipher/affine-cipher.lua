local validate, encipher, add_spaces, mmi
local M = 26  -- size of alphabet

local function encode(phrase, key)
  validate(key.a)
  local encoder = function (x) return (key.a * x + key.b) % M end
  return add_spaces(5, encipher(phrase, encoder))
end

local function decode(phrase, key)
  validate(key.a)
  local aa = mmi(key.a, M)
  local decoder = function (y) return (aa * (y - key.b)) % M end
  return encipher(phrase, decoder)
end

-- -------------------------------------------------------------
local gcd, ord, chr

validate = function(a)
  assert(gcd(a, M) == 1, 'a and m must be coprime.')
end

gcd = function (a, b)
  while b ~= 0 do a, b = b, a % b end
  return a
end

mmi = function (a, m)
  for x = 1, m do
    if (a * x) % m == 1 then return x end
  end
  return 0
end

ord = function (letter) return letter:byte() end
chr = function (number) return string.char(number) end
local A = ord('a')

encipher = function (text, func) 
  local cipher = ''
  for char in text:lower():gmatch('[%a%d]') do
    cipher = cipher .. (char:match('%d') and char or chr(A + func(ord(char) - A)))
  end
  return cipher
end

add_spaces = function (size, text)
  local chunks = {}
  while text ~= '' do
    table.insert(chunks, text:sub(1, size))
    text = text:sub(size + 1)
  end
  return table.concat(chunks, ' ')
end
-- -------------------------------------------------------------

return { encode = encode, decode = decode }
