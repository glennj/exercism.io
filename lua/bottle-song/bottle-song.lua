local NUMBERS = {[0] = 'No', 'One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten'}

function bottles(n)
  return string.format('%s green bottle%s',
    NUMBERS[n],
    n == 1 and '' or 's'
  )
end

function verse(n)
  local v = {}
  v[1] = string.format('%s hanging on the wall,', bottles(n))
  v[2] = v[1]
  v[3] = 'And if one green bottle should accidentally fall,'
  v[4] = string.format('There\'ll be %s hanging on the wall.', bottles(n-1):lower())
  return table.concat(v, '\n') .. '\n'
end

return {
  recite = function(start_bottles, take_down)
    local vs = {}
    for n = start_bottles, (start_bottles - take_down + 1), -1 do
      table.insert(vs, verse(n))
    end
    return table.concat(vs, '\n')
  end
}
