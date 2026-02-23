colors = -> {
  'black', 'brown', 'red', 'orange', 'yellow',
  'green',  'blue', 'violet', 'grey', 'white'
}


color_code = (color) ->
  for i, c in ipairs colors!
    if c == color
      return i - 1
  return -1
  

{ :colors, :color_code }
