export class Triangle {
  ok: boolean
  uniqSides: Set<number>

  constructor(...sides: number[]) {
    const [a, b, c] = sides.sort((a, b) => a - b)
    this.ok = a >= 0 && c > 0 && a + b >= c    // triangle inequality
    this.uniqSides = new Set(sides)
  }

  get isEquilateral() { return this.ok && this.uniqSides.size === 1 }
  get isIsosceles()   { return this.ok && this.uniqSides.size < 3 }
  get isScalene()     { return this.ok && this.uniqSides.size === 3 }
}
