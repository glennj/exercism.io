use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: leap end

fun leap(year):
  has-factor = lam(n, f): num-modulo(n, f) == 0 end
  is-factor = has-factor(year, _)
  is-factor(4) and (not(is-factor(100)) or is-factor(400))
end
