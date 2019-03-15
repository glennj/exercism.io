local split = require("split")  -- https://luarocks.org/modules/telemachus/split

local DIGIT_STRINGS = {
  [' _ | ||_|   '] = 0, ['     |  |   '] = 1,
  [' _  _||_    '] = 2, [' _  _| _|   '] = 3,
  ['   |_|  |   '] = 4, [' _ |_  _|   '] = 5,
  [' _ |_ |_|   '] = 6, [' _   |  |   '] = 7,
  [' _ |_||_|   '] = 8, [' _ |_| _|   '] = 9,
}
local DIM = {w = 3, h = 4}

local validate
local each_slice    -- name inspired by ruby
local convert_ocr_line
local digit_strings
 
local convert = function (input)
  if (input or "") == "" then return "" end

  local lines = split.split(input, "\n")
  validate(lines)

  local result = {}
  -- an OCR "line" is a list of 4 lines of text
  for line in each_slice(lines, DIM.h) do
    result[#result+1] = convert_ocr_line(line)
  end
  return table.concat(result, ',')
end

validate = function (lines)
  assert(#lines % DIM.h == 0)
  for i, line in ipairs(lines) do
    assert(#lines[i] == #lines[1] and #lines[i] % DIM.w == 0)
  end
end

convert_ocr_line = function(line)
  local digits = ""
  for s in digit_strings(line) do
    digits = digits .. (DIGIT_STRINGS[s] or '?')
  end
  return digits
end 

digit_strings = function(line)
  return coroutine.wrap(function()
    for i = 1, #line[1] // DIM.w do
      local digit_string = ""
      for j = 1, DIM.h do
        local s = line[j]:sub((i-1)*DIM.w+1, i*DIM.w)
        digit_string = digit_string .. s
      end
      coroutine.yield(digit_string)
    end
  end)
end

-- iterate over sublists of a list
-- example
--[[
        > list = {'a','b','c','d','e','f','g','h'}
        > for slice in each_slice(list, 3) do
        >> print(table.unpack(slice))
        >> end
        a	b	c
        d	e	f
        g	h
-]]
each_slice = function (list, size)
  return coroutine.wrap(function()
    for i = 1, math.ceil(#list / size) do
      local slice = {}
      for j = (i-1)*size+1, i*size do
        slice[#slice+1] = list[j]
      end
      coroutine.yield(slice)
    end
  end)
end

return { convert = convert }
