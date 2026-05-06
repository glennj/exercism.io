local color_code = require 'resistor-color'.color_code

return {
  value = function(colors)
    return 10 * color_code(colors[1]) + color_code(colors[2])
  end
}
