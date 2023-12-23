use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: label end

import file("resistor-color.arr") as RC
import file("resistor-color-duo.arr") as RC2

fun label(colors):
  code = RC2.color-code(colors.take(2))
         * num-expt(10, RC.color-code(colors.get(2)))

  rec reduce = lam(val, idx):
    if (val > 0) and (num-modulo(val, 1000) == 0):
      reduce(val / 1000, idx + 1)
    else:
      {val; idx}
    end
  end
  {value; index} = reduce(code, 0)

  "{1} {2}ohms"
  ^ string-replace(_, "{1}", num-to-string(value))
  ^ string-replace(_, "{2}", [list: "", "kilo", "mega", "giga"].get(index))
end
