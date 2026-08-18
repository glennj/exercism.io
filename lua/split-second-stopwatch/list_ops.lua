local function fold(t, init, f)
  local acc = init
  for _, elem in ipairs(t) do
    acc = f(acc, elem)
  end
  return acc
end

local function sum(t)
  return fold(t, 0, function(acc, elem) return acc + elem end)
end

local function map(t, f)
  return fold(t, {}, function(acc, elem) 
    table.insert(acc, f(elem))
    return acc
  end)
end

return {
  fold = fold,
  sum = sum,
  map = map,
}
