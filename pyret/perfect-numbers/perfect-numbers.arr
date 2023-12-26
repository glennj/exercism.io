use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: classify end

import sets as S
import math as M

fun classify(number):
  if number < 1:
    raise("Classification is only possible for positive integers.")
  else:
    sum = aliquot-sum(number)
    ask:
      | sum < number then: 'deficient'
      | sum > number then: 'abundant'
      | otherwise: 'perfect'
    end
  end
end

fun aliquot-sum(number):
  limit = num-floor(num-sqrt(number))
  rec factors = lam(f, fs):
    ask:
      | f > limit then: fs
      | not(num-modulo(number, f) == 0) then: factors(f + 1, fs)
      | otherwise: factors(f + 1, fs.add(f).add(number / f))
    end
  end
  factors(1, S.empty-tree-set)
    .remove(number)
    .to-list()
  ^ M.sum(_)
end
