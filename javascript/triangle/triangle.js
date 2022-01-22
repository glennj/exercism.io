export class Triangle {
  constructor(...args) {
    const [a, b, c] = args.sort((i, j) => i - j);  // sort numerically
    this.isTriangle = args.every(n => n > 0) && a + b > c;
    this.sides = new Set(args);
  }

  get isEquilateral() { return this.isTriangle && this.sides.size === 1; }
  get isIsosceles()   { return this.isTriangle && this.sides.size < 3; }
  get isScalene()     { return this.isTriangle && this.sides.size === 3; }
}
