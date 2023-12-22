use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: color-code end

import file("resistor-color.arr") as RC

fun color-code(colors):
  colors
    .take(2)
    .foldl({(color, code): (10 * code) + RC.color-code(color)}, 0)
end
