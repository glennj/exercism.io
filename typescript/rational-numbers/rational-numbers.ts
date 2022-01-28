const gcd = (a: number, b: number): number => (b ? gcd(b, a % b) : a)

const reduced = (aa: number, bb: number): number[] => {
  const g = gcd(aa, bb)
  const [a, b] = [aa / g, bb / g]
  return b < 0 ? [-a, -b] : [a, b]
}

const nthRoot = (num: number, root: number): number => {
  return Math.E ** (Math.log(num) / root)
}

export class Rational {
  private num: number
  private den: number

  constructor(numerator: number, denominator: number) {
    [this.num, this.den] = reduced(numerator, denominator)
  }

  add(other: Rational): Rational {
    const newNum = this.num * other.den + this.den * other.num
    const newDen = this.den * other.den
    return new Rational(newNum, newDen)
  }

  sub(other: Rational): Rational {
    const newNum = this.num * other.den - this.den * other.num
    const newDen = this.den * other.den
    return new Rational(newNum, newDen)
  }

  mul(other: Rational): Rational {
    const newNum = this.num * other.num
    const newDen = this.den * other.den
    return new Rational(newNum, newDen)
  }

  div(other: Rational): Rational {
    const newNum = this.num * other.den
    const newDen = this.den * other.num
    return new Rational(newNum, newDen)
  }

  abs(): Rational {
    return new Rational(Math.abs(this.num), Math.abs(this.den))
  }

  exprational(n: number): Rational {
    if (n < 0) {
      return new Rational((this.den ** -n), (this.num ** -n))
    }
    return new Rational((this.num ** n), (this.den ** n))
  }

  toFloat(): number {
    return this.num / this.den
  }

  expreal(n: number): number {
    const result = nthRoot(n ** this.num, this.den)
    return Number(result.toPrecision(15))
  }

  reduce(): Rational {
    // the constructor reduces already.
    return this
  }
}
