function seq(n: number): number[] {
  return Array.from({length: n + 1}, (_, i) => i)
}

export function primes(limit: number): number[] {
  const p: number[] = seq(limit)
  p[0] = 0
  p[1] = 0

  const removeMultiplesOf = (n: number): void => {
    const step = n === 2 ? 2 : n * 2
    for (let m = n * n; m <= limit; m += step) {
      p[m] = 0
    }
  }

  removeMultiplesOf(2)
  for (let n = 3; n * n <= limit; n += 2) {
    removeMultiplesOf(n)
  }

  return p.filter(v => v !== 0)
}
