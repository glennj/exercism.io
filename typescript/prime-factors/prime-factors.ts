function calculatePrimeFactors(num: number): number[] {
  const factors: number[] = []

  while (num % 2 === 0) {
    factors.push(2)
    num /= 2
  }

  let f = 3
  while (f * f <= num) {
    if (num % f === 0) {
      factors.push(f)
      num /= f
    } else {
      f += 2
    }
  }

  if (num > 1) {factors.push(num)}
  return factors
}

export default calculatePrimeFactors
