use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: is-valid end

import lists as L
include file("string-helpers.arr")

fun is-valid(card-number):
  doubled = [list: [list: 0,1,2,3,4,5,6,7,8,9],
                   [list: 0,2,4,6,8,1,3,5,7,9]]
  digits = card-number
           ^ string-explode(_)
           ^ filter({(c): not(string-is-whitespace(c))}, _)
  ask:
    | digits.any({(c): not(string-is-digit(c))}) then: false
    | digits.length() <= 1 then: false
    | otherwise:
        sum = digits
              ^ map({(c): string-to-number(c).value}, _)
              ^ L.foldr(
                  lam({sum; double}, digit): 
                    { sum + doubled.get(double).get(digit);
                      num-modulo(double + 1, 2) }
                  end,
                  {0; 0}, _)
        num-modulo(sum.{0}, 10) == 0
  end
end
