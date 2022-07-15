class ComplexNumber {
  construct new(real, imaginary) {
    _real = real
    _imag = imaginary
  }

  real {_real}
  imag {_imag}

  toComplex(other) { other.type == type ? other : type.new(other, 0) }

  ==(other) { type == other.type && real == other.real && imag == other.imag }

  +(other) { 
    other = toComplex(other)
    return type.new(real + other.real, imag + other.imag)
  }

  -(other) {
    other = toComplex(other)
    return type.new(real - other.real, imag - other.imag) 
  }

  *(other) {
    other = toComplex(other)
    var r = real * other.real - imag * other.imag
    var i = imag * other.real + real * other.imag
    return type.new(r, i)
  }

  /(other) {
    other = toComplex(other)
    var div = other.real.pow(2) + other.imag.pow(2)
    var r = (real * other.real + imag * other.imag) / div
    var i = (imag * other.real - real * other.imag) / div
    return type.new(r, i)
  }

  abs  { (real.pow(2) + imag.pow(2)).sqrt }
  conj { type.new(real, -imag) }
  exp  { type.new(1.exp.pow(real), 0) * type.new(imag.cos, imag.sin) }
}
