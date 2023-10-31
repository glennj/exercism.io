local rc = require 'resistor-color'

return {
  value = function(colors)
    return 10 * rc.color_code(colors[1]) + rc.color_code(colors[2])
  end
}
