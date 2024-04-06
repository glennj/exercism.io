use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: primes end

fun primes(limit) block:
  marks = array-of(true, limit + 1)
  marks.set-now(0, false)
  marks.set-now(1, false)

  # mark multiples of 2
  for each(m from range-by(2 * 2, limit + 1, 2)):
    marks.set-now(m, false)
  end

  # mark multiples of other primes
  for each(p from range-by(3, num-sqrt(limit), 2)):
    when marks.get-now(p):
      for each(m from range-by(p * p, limit + 1, 2 * p)):
        marks.set-now(m, false)
      end
    end
  end

  marks.to-list-now()
  ^ map_n(lam(i, p): {i; p} end, 0, _)
  ^ filter(lam(pair): pair.{1} end, _)
  ^ map(lam(pair): pair.{0} end, _)

end
