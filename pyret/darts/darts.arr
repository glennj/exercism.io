use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: score end

fun score(x, y):
  dist = num-sqrt(num-sqr(x) + num-sqr(y))
  ask:
    | dist <= 1 then: 10
    | dist <= 5 then: 5
    | dist <= 10 then: 1
    | otherwise: 0
  end
end
