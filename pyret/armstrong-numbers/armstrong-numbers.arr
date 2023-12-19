use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: is-armstrong-number end

fun is-armstrong-number(number):
  rec armstrong-sum = lam(num, len, sum):
    if num == 0:
      sum
    else:
      digit = num-modulo(num, 10)
      armstrong-sum((num - digit) / 10, len, sum + num-expt(digit, len))
    end
  end

  ask:
    | number == 0 then: true
    | otherwise:
        length = num-floor(1 + (num-log(number) / num-log(10)))
        number == armstrong-sum(number, length, 0)
  end
end
