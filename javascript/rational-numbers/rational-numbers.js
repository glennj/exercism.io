const gcd = (a, b) => (b ? gcd(b, a % b) : a);

const reduced = (aa, bb) => {
  const g = gcd(aa, bb);
  const [a, b] = [aa / g, bb / g];
  return b < 0 ? [-a, -b] : [a, b];
};

const nthRoot = (num, root) => Math.E ** (Math.log(num) / root);

class Rational {
  constructor(numerator, denominator) {
    [this.num, this.den] = reduced(numerator, denominator);
  }

  add(other) {
    const newNum = this.num * other.den + this.den * other.num;
    const newDen = this.den * other.den;
    return new Rational(newNum, newDen);
  }

  sub(other) {
    const newNum = this.num * other.den - this.den * other.num;
    const newDen = this.den * other.den;
    return new Rational(newNum, newDen);
  }

  mul(other) {
    const newNum = this.num * other.num;
    const newDen = this.den * other.den;
    return new Rational(newNum, newDen);
  }

  div(other) {
    const newNum = this.num * other.den;
    const newDen = this.den * other.num;
    return new Rational(newNum, newDen);
  }

  abs() {
    return new Rational(Math.abs(this.num), Math.abs(this.den));
  }

  exprational(n) {
    if (n < 0) {
      return new Rational((this.den ** -n), (this.num ** -n));
    }
    return new Rational((this.num ** n), (this.den ** n));
  }

  toFloat() {
    return this.num / this.den;
  }

  // Testing note: use toBeCloseTo(), not toEqual()
  expreal(n) {
    // return n ** this.toFloat();
    return nthRoot(n ** this.num, this.den);
  }

  reduce() {
    // the constructor reduces already.
    return this;
  }
}

module.exports = { Rational };
