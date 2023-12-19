use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: steps end

fun steps(number) block:
  when num-is-non-positive(number):
    raise("Only positive numbers are allowed")
  end

  num-is-even = lam(num): num-modulo(num, 2) == 0 end

  rec stepper = lam(num, step):
    ask:
      | num == 1 then: step
      | num-is-even(num) then: stepper(num / 2, step + 1)
      | otherwise: stepper((3 * num) + 1, step + 1)
    end
  end

  stepper(number, 0)
end
