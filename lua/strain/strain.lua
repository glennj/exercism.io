local keep = function(xs, pred)
  local result = {}

  for _,x in ipairs(xs) do
    if pred(x) then
      table.insert(result, x)
    end
  end

  return result
end


local discard = function(xs, pred)
  return keep(xs, function(x) return not pred(x) end)
end


return {
  keep = keep,
  discard = discard
}
