class Rational {
  construct new(numerator, denominator) {
    var g = Math.gcd(numerator.abs, denominator.abs)
    var m = denominator < 0 ? -1 : 1
    _n = m * numerator / g
    _d = m * denominator / g
  }

  num {_n}
  den {_d}
  toString { "%(type)(%(num), %(den))" }

  ==(other) { type == other.type && num == other.num && den == other.den }

  +(other) { type.new(num * other.den + den * other.num, den * other.den) }
  *(other) { type.new(num * other.num, den * other.den) }

  negated { type.new(-num, den) }
  -(other) { this + other.negated }

  inverted { type.new(den, num) }
  /(other) { this * other.inverted }

  abs { type.new(num.abs, den) }

  pow(x) {
    var r = x < 0 ? inverted : this
    return type.new(r.num.pow(x.abs), r.den.pow(x.abs))
  }

  expreal(x) { Math.nthroot(x.pow(num), den)}
}

class Math {
  static gcd(a, b) { b == 0 ? a : gcd(b, a % b)}
  static nthroot(num, root) { 1.exp.pow(num.log / root) }
}