use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: on-square, total end

SIZE = 64

fun on-square(n) block:
  when num-is-non-positive(n) or (n > SIZE):
    raise("square must be between 1 and 64")
  end

  num-expt(2, n - 1)
end

fun total():
  num-expt(2, SIZE) - 1
end
