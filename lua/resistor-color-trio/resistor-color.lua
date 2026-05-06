local colors = {
  black = 0, brown = 1, red    = 2, orange = 3, yellow = 4,
  green = 5, blue  = 6, violet = 7, grey   = 8, white  = 9,
}

return {
  color_code = function(color)
    return colors[color]
  end,

  colors = function()
    local cs = {}
    for c, v in pairs(colors) do
      cs[v + 1] = c
    end
    return cs
  end
}
