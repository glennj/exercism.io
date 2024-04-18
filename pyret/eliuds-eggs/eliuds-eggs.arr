use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: egg-count end

fun egg-count(n):
  rec counter = lam(num, count):
    if num == 0:
      count
    else:
      egg = num-modulo(num, 2)
      next = num-floor(num / 2)
      counter(next, count + egg)
    end
  end
  counter(n, 0)
end
