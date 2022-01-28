import {from} from './iterable-range'

// factorial, cached
const factCache: number[] = []
function fact(n: number): number {
  if (factCache[n] !== undefined) { return factCache[n] }
  if (n === 1) { return 1 }
  const f = n * fact(n - 1)
  factCache[n] = f
  return f
}

// binomial coefficient, or "n choose k"
function binom(n: number, k: number): number {
  if (k === 0 || n - k === 0) { return 1 }
  if (k === 1 || n - k === 1) { return n }
  return fact(n) / ( fact(k) * fact(n - k) )
}

export class Triangle {
  rows: number[][]

  constructor(n: number) {
    this.rows = from(0).upTo(n - 1).map(i => this.row(i))
  }

  private row(n: number): number[] {
    return from(0).upTo(n).map(k => binom(n, k))
  }

  get lastRow(): number[] {
    return this.rows[this.rows.length - 1]
  }
}
