local ordinals = {
  'first', 'second', 'third', 'fourth', 'fifth', 'sixth',
  'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth',
}

local gifts = {
  'a Partridge in a Pear Tree.', 'two Turtle Doves', 'three French Hens',
  'four Calling Birds', 'five Gold Rings', 'six Geese-a-Laying',
  'seven Swans-a-Swimming', 'eight Maids-a-Milking', 'nine Ladies Dancing',
  'ten Lords-a-Leaping', 'eleven Pipers Piping', 'twelve Drummers Drumming',
}

local function verse(n)
  local v = 'On the ' .. ordinals[n] .. ' day of Christmas my true love gave to me: '
  local gs = { (n > 1 and 'and ' or '') .. gifts[1] }
  for i = 2, n do
    table.insert(gs, 1, gifts[i])
  end
  return v .. table.concat(gs, ', ')
end

local function recite(start_verse, end_verse)
  local vs = {}
  for i = start_verse, end_verse do
    table.insert(vs, verse(i))
  end
  return vs
end

return { recite = recite }
