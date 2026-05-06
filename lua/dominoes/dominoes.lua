-- https://lunarmodules.github.io/Penlight/classes/pl.List.html
-- available as a dependency of `busted`
local List = require('pl.List')

local function is_chain(chain)
  return #chain == 0 or chain[1][1] == chain[#chain][2]
end
    
local function buildChain(chain, rest)
  if #rest == 0 then
    return is_chain(chain)
  end

  local last = chain[#chain][2]
  for i, d in ipairs(rest) do
    if last == d[1] or last == d[2] then
      if last == d[2] then
        d = {d[2], d[1]}
      end
      if buildChain(chain:clone():append(d), rest:clone():remove(i)) then
        return true
      end
    end
  end
  return false
end

return {
  can_chain = function(dominoes)
    ds = List(dominoes)
    return buildChain(ds:slice(1,1), ds:slice(2))
  end
}
