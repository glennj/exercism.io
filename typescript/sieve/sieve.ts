function primes(limit: number): number[] {
  const p = Array.from({length: 1 + limit}, (_, i) => i)
  p[1] = 0

  const removeMultiplesOf = (n: number): void => {
    const step = n * (n === 2 ? 1 : 2)
    for (let m = n * n; m <= limit; m += step) {
      p[m] = 0
    }
  }

  removeMultiplesOf(2)
  for (let n = 3; n * n <= limit; n += 2) {
    // if n is the square of some other number, it's already been handled
    if (!Number.isInteger(Math.sqrt(n))) {
      removeMultiplesOf(n)
    }
  }

  return p.filter((v) => v > 0)
}

export default { primes }
