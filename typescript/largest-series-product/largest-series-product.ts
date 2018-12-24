class Series {
  private data: number[]

  constructor(input: string) {
    if (/\D/.test(input)) { throw new Error('Invalid input.') }
    this.data = input.split('').map(Number)
  }

  *slices(len: number = 1): IterableIterator<number[]> {
    for (let i = 0; i <= this.data.length - len; i++) {
      yield this.data.slice(i, i + len)
    }
  }

  largestProduct(len: number): number {
    if (len < 0) { throw new Error('Invalid input.') }
    if (this.data.length < len) { throw new Error('Slice size is too big.') }
    let maxProduct = -Infinity
    for (const s of this.slices(len)) {
      const product = s.reduce((p, d) => p * d, 1)
      if (product > maxProduct) { maxProduct = product }
    }
    return maxProduct
  }
}

export default Series
