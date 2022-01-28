type Triplet = [number, number, number]

class Triangle {
  private edges: Triplet

  constructor(...edges: Triplet) {
    this.edges = edges.sort((a, b) => a - b)
  }

  get perimeter(): number {
    return this.edges.reduce((u, v) => u + v)
  }

  isPythagorean(): boolean {
    const [a, b, c] = this.edges
    return (c ** 2) === (a ** 2) + (b ** 2)
  }

  toString(): string {
    return `[${this.edges.join(',')}]`
  }
  toArray(): [number, number, number] {
    return this.edges
  }

  static where(maxFactor: number, minFactor?: number, sum?: number): Triangle[] {
    const [min, max] = [minFactor ?? 1, maxFactor]
    const result = []
    for (let c = max; c >= min; c--) {
      for (let b = c - 1; b >= min; b--) {
        const a = Math.sqrt((c ** 2) - (b ** 2))
        if (Number.isInteger(a) && a > b) {
          const t = new Triangle(a, b, c)
          if (t.isPythagorean() && (!sum || sum === t.perimeter)) {
            result.push(t)
          }
        }
      }
    }
    return result
  }
}


type Options = {sum: number, maxFactor?: number, minFactor?: number}

export function triplets(options: Options): Triangle[] {
  const sum = options.sum
  const min = options.minFactor ??= 1
  const max = options.maxFactor ??= Math.floor(sum / 2)
  return Triangle.where(max, min, sum)
}
