const isPrime = (n: number): boolean => {
  if (n < 2) { return false }
  if (n === 2) { return true }
  if (n % 2 === 0) { return false }
  for (let i = 3; i * i <= n; i += 2) {
    if (n % i === 0) { return false }
  }
  return true
}

function* PrimeNumberGenerator() {
  yield 2
  yield 3
  let p = 3
  while (true) {
    do { p += 2 } while (!isPrime(p))
    yield p
  }
}

class Prime {
  private readonly primes: number[]
  private readonly png: Iterator<number>

  constructor() {
    this.primes = []
    this.png = PrimeNumberGenerator()
  }

  nth(n: number): number {
    if (n === 0) { throw new Error('Prime is not possible') }
    while (this.primes.length < n) {
      const p = this.png.next().value
      this.primes.push(p)
    }
    return this.primes[n - 1]
  }
}

export default Prime
