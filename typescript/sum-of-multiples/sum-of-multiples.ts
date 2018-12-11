type CanTo = { to: (n: number) => number }

function SumOfMultiples(factors: number[]): CanTo {
  // instantiate an anonymous class
  return new class {
    to(n: number): number {
      const multiples: Set<number> = new Set()
      factors.forEach((f) => {
        for (let i = 1; i * f < n; i++) {
          multiples.add(i * f)
        }
      })
      let sum = 0
      for (const multiple of multiples) { sum += multiple }
      return sum
    }
  }()
}

export default SumOfMultiples
