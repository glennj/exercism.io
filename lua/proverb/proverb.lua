local Proverb = {}

function Proverb.recite(strings)
  local first = function(i)
    return string.format("For want of a %s the %s was lost.\n", strings[i-1], strings[i])
  end
  local last = function()
    return string.format("And all for the want of a %s.\n", strings[1])
  end
  
  local lines = {}
  for i = 2, #strings do table.insert(lines, first(i)) end
  if #strings > 0 then table.insert(lines, last()) end
  
  return table.concat(lines)
end

return Proverb
