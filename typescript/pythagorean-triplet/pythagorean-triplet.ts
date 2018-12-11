class Triplet {
  private _sum: number
  private _product: number
  private edges: [number, number, number]

  constructor(a: number, b: number, c: number) {
    if (c <= a || c <= b) {
      throw new Error('the hypoteneuse is too short')
    }
    this._sum = a + b + c
    this._product = a * b * c
    this.edges = [a, b, c]
  }

  sum():     number { return this._sum }
  product(): number { return this._product }

  isPythagorean(): boolean {
    const [a, b, c] = this.edges
    return (c ** 2) === (a ** 2) + (b ** 2)
  }

  toString(): string {
    return `[${this.edges.join(',')}]`
  }

  static where(maxFactor: number, minFactor?: number, sum?: number): Triplet[] {
    const [min, max] = [minFactor || 1, maxFactor]
    const result = []
    for (let c = max; c >= min; c--) {
      for (let b = c - 1; b >= min; b--) {
        const a = Math.sqrt((c ** 2) - (b ** 2))
        if (Number.isInteger(a) && a > b) {
          const t = new Triplet(a, b, c)
          if (t.isPythagorean() && (!sum || sum === t.sum())) {
            result.push(t)
          }
        }
      }
    }
    return result
  }
}

export default Triplet
