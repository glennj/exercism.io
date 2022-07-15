import "wren-testie/testie" for Testie, Expect
import "./complex-numbers" for ComplexNumber

class Math {
  static approxEqual(actual, expected) { 
    var epsilon = 1e-6
    return (actual - expected).abs < epsilon
  }
}

Testie.test("Complex Numbers") { |do, skip|

  do.describe("Real part") {
    do.test("Real part of a purely real number") {
      var c = ComplexNumber.new(1, 0)
      Expect.value(c.real).toEqual(1)
    }

    do.test("Real part of a purely imaginary number") {
      var c = ComplexNumber.new(0, 1)
      Expect.value(c.real).toEqual(0)
    }

    do.test("Real part of a number with real and imaginary part") {
      var c = ComplexNumber.new(1, 2)
      Expect.value(c.real).toEqual(1)
    }
  }

  do.describe("Imaginary part") {
    do.test("Imaginary part of a purely real number") {
      var c = ComplexNumber.new(1, 0)
      Expect.value(c.imag).toEqual(0)
    }

    do.test("Imaginary part of a purely imaginary number") {
      var c = ComplexNumber.new(0, 1)
      Expect.value(c.imag).toEqual(1)
    }

    do.test("Imaginary part of a number with real and imaginary part") {
      var c = ComplexNumber.new(1, 2)
      Expect.value(c.imag).toEqual(2)
    }
  }

  do.describe("Addition") {
    do.test("Add purely real numbers") {
      var c1 = ComplexNumber.new(1, 0)
      var c2 = ComplexNumber.new(2, 0)
      Expect.value(c1 + c2).toEqual(ComplexNumber.new(3, 0))
    }

    do.test("Add purely imaginary numbers") {
      var c1 = ComplexNumber.new(0, 1)
      var c2 = ComplexNumber.new(0, 2)
      Expect.value(c1 + c2).toEqual(ComplexNumber.new(0, 3))
    }

    do.test("Add numbers with real and imaginary part") {
      var c1 = ComplexNumber.new(1, 2)
      var c2 = ComplexNumber.new(3, 4)
      Expect.value(c1 + c2).toEqual(ComplexNumber.new(4, 6))
    }
  }

  do.describe("Subtraction") {
    do.test("Subtract purely real numbers") {
      var c1 = ComplexNumber.new(1, 0)
      var c2 = ComplexNumber.new(2, 0)
      Expect.value(c1 - c2).toEqual(ComplexNumber.new(-1, 0))
    }

    do.test("Subtract purely imaginary numbers") {
      var c1 = ComplexNumber.new(0, 1)
      var c2 = ComplexNumber.new(0, 2)
      Expect.value(c1 - c2).toEqual(ComplexNumber.new(0, -1))
    }

    do.test("Subtract numbers with real and imaginary part") {
      var c1 = ComplexNumber.new(1, 2)
      var c2 = ComplexNumber.new(3, 4)
      Expect.value(c1 - c2).toEqual(ComplexNumber.new(-2, -2))
    }
  }

  do.describe("Multiplication") {
    do.test("Multiply purely real numbers") {
      var c1 = ComplexNumber.new(1, 0)
      var c2 = ComplexNumber.new(2, 0)
      Expect.value(c1 * c2).toEqual(ComplexNumber.new(2, 0))
    }

    do.test("Multiply purely imaginary numbers") {
      var c1 = ComplexNumber.new(0, 1)
      var c2 = ComplexNumber.new(0, 2)
      Expect.value(c1 * c2).toEqual(ComplexNumber.new(-2, 0))
    }

    do.test("Multiply numbers with real and imaginary part") {
      var c1 = ComplexNumber.new(1, 2)
      var c2 = ComplexNumber.new(3, 4)
      Expect.value(c1 * c2).toEqual(ComplexNumber.new(-5, 10))
    }

    do.test("Imaginary unit") {
      var c1 = ComplexNumber.new(0, 1)
      var c2 = ComplexNumber.new(0, 1)
      Expect.value(c1 * c2).toEqual(ComplexNumber.new(-1, 0))
    }
  }

  do.describe("Division") {
    do.test("Divide purely real numbers") {
      var c1 = ComplexNumber.new(1, 0)
      var c2 = ComplexNumber.new(2, 0)
      Expect.value(c1 / c2).toEqual(ComplexNumber.new(0.5, 0.0))
    }

    do.test("Divide purely imaginary numbers") {
      var c1 = ComplexNumber.new(0, 1)
      var c2 = ComplexNumber.new(0, 2)
      Expect.value(c1 / c2).toEqual(ComplexNumber.new(0.5, 0.0))
    }

    do.test("Divide numbers with real and imaginary part") {
      var c1 = ComplexNumber.new(1, 2)
      var c2 = ComplexNumber.new(3, 4)
      Expect.value(c1 / c2).toEqual(ComplexNumber.new(0.44, 0.08))
    }
  }

  do.describe("Absolute Value") {
    do.test("Absolute value of a positive purely real number") {
      var c = ComplexNumber.new(5, 0)
      Expect.value(c.abs).toEqual(5.0)
    }

    do.test("Absolute value of a negative purely real number") {
      var c = ComplexNumber.new(-5, 0)
      Expect.value(c.abs).toEqual(5.0)
    }

    do.test("Absolute value of a purely imaginary number with positive imaginary part") {
      var c = ComplexNumber.new(0, 5)
      Expect.value(c.abs).toEqual(5.0)
    }

    do.test("Absolute value of a purely imaginary number with negative imaginary part") {
      var c = ComplexNumber.new(0, -5)
      Expect.value(c.abs).toEqual(5.0)
    }

    do.test("Absolute value of a number with real and imaginary part") {
      var c = ComplexNumber.new(3, 4)
      Expect.value(c.abs).toEqual(5.0)
    }
  }

  do.describe("Complex conjugate") {
    do.test("Conjugate a purely real number") {
      var c = ComplexNumber.new(5, 0)
      Expect.value(c.conj).toEqual(ComplexNumber.new(5, 0))
    }

    do.test("Conjugate a purely imaginary number") {
      var c = ComplexNumber.new(0, 5)
      Expect.value(c.conj).toEqual(ComplexNumber.new(0, -5))
    }

    do.test("Conjugate a number with real and imaginary part") {
      var c = ComplexNumber.new(1, 1)
      Expect.value(c.conj).toEqual(ComplexNumber.new(1, -1))
    }
  }

  do.describe("Complex exponential function") {
    do.test("Euler's identity/formula") {
      var c = ComplexNumber.new(0, Num.pi).exp
      Expect.value(Math.approxEqual(c.real, -1)).toBe(true)
      Expect.value(Math.approxEqual(c.imag, 0)).toBe(true)
    }

    do.test("Exponential of 0") {
      var c = ComplexNumber.new(0, 0).exp
      Expect.value(Math.approxEqual(c.real, 1)).toBe(true)
      Expect.value(Math.approxEqual(c.imag, 0)).toBe(true)
    }

    do.test("Exponential of a purely real number") {
      var c = ComplexNumber.new(1, 0).exp
      Expect.value(Math.approxEqual(c.real, 1.exp)).toBe(true)
      Expect.value(Math.approxEqual(c.imag, 0)).toBe(true)
    }

    do.test("Exponential of a number with real and imaginary part") {
      var c = ComplexNumber.new(2.log, Num.pi).exp
      Expect.value(Math.approxEqual(c.real, -2)).toBe(true)
      Expect.value(Math.approxEqual(c.imag, 0)).toBe(true)
    }
  }

  do.describe("Operations between real numbers and complex numbers") {
    /* tests where the receiver is a Num are not implemented:
     *   "Add complex number to real number"
     *   "Subtract complex number from real number"
     *   "Multiply real number by complex number"
     *   "Divide real number by complex number"
     */
    do.test("Add real number to complex number") {
      var a = ComplexNumber.new(1, 2)
      var b = 5
      Expect.value(a + b).toEqual(ComplexNumber.new(6, 2))
    }
    do.test("Subtract real number from complex number") {
      var a = ComplexNumber.new(5, 7)
      var b = 4
      Expect.value(a - b).toEqual(ComplexNumber.new(1, 7))
    }
    do.test("Multiply complex number by real number") {
      var a = ComplexNumber.new(2, 5)
      var b = 5
      Expect.value(a * b).toEqual(ComplexNumber.new(10, 25))
    }
    do.test("Divide complex number by real number") {
      var a = ComplexNumber.new(10, 100)
      var b = 10
      Expect.value(a / b).toEqual(ComplexNumber.new(1, 10))
    }
  }
}
