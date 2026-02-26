import ComplexNumber, add, sub, mul, div from require 'complex_numbers'

describe 'complex-numbers', ->
  pi = math.pi
  e  = math.exp(1)
  ln = math.log

  describe 'Real part', ->
    it 'Real part of a purely real number', ->
      c = ComplexNumber(1, 0)
      result = c\real!
      assert.are.equal 1, result

    it 'Real part of a purely imaginary number', ->
      c = ComplexNumber(0, 1)
      result = c\real!
      assert.are.equal 0, result

    it 'Real part of a number with real and imaginary part', ->
      c = ComplexNumber(1, 2)
      result = c\real!
      assert.are.equal 1, result

  describe 'Imaginary part', ->
    it 'Imaginary part of a purely real number', ->
      c = ComplexNumber(1, 0)
      result = c\imaginary!
      assert.are.equal 0, result

    it 'Imaginary part of a purely imaginary number', ->
      c = ComplexNumber(0, 1)
      result = c\imaginary!
      assert.are.equal 1, result

    it 'Imaginary part of a number with real and imaginary part', ->
      c = ComplexNumber(1, 2)
      result = c\imaginary!
      assert.are.equal 2, result

  it 'Imaginary unit', ->
    c1 = ComplexNumber(0, 1)
    c2 = ComplexNumber(0, 1)
    result = c1 * c2
    expected = ComplexNumber(-1, 0)
    assert.are.equal expected, result

  describe 'Arithmetic', ->
    describe 'Addition', ->
      it 'Add purely real numbers', ->
        c1 = ComplexNumber(1, 0)
        c2 = ComplexNumber(2, 0)
        result = c1 + c2
        expected = ComplexNumber(3, 0)
        assert.are.equal expected, result

      it 'Add purely imaginary numbers', ->
        c1 = ComplexNumber(0, 1)
        c2 = ComplexNumber(0, 2)
        result = c1 + c2
        expected = ComplexNumber(0, 3)
        assert.are.equal expected, result

      it 'Add numbers with real and imaginary part', ->
        c1 = ComplexNumber(1, 2)
        c2 = ComplexNumber(3, 4)
        result = c1 + c2
        expected = ComplexNumber(4, 6)
        assert.are.equal expected, result

    describe 'Subtraction', ->
      it 'Subtract purely real numbers', ->
        c1 = ComplexNumber(1, 0)
        c2 = ComplexNumber(2, 0)
        result = c1 - c2
        expected = ComplexNumber(-1, 0)
        assert.are.equal expected, result

      it 'Subtract purely imaginary numbers', ->
        c1 = ComplexNumber(0, 1)
        c2 = ComplexNumber(0, 2)
        result = c1 - c2
        expected = ComplexNumber(0, -1)
        assert.are.equal expected, result

      it 'Subtract numbers with real and imaginary part', ->
        c1 = ComplexNumber(1, 2)
        c2 = ComplexNumber(3, 4)
        result = c1 - c2
        expected = ComplexNumber(-2, -2)
        assert.are.equal expected, result

    describe 'Multiplication', ->
      it 'Multiply purely real numbers', ->
        c1 = ComplexNumber(1, 0)
        c2 = ComplexNumber(2, 0)
        result = c1 * c2
        expected = ComplexNumber(2, 0)
        assert.are.equal expected, result

      it 'Multiply purely imaginary numbers', ->
        c1 = ComplexNumber(0, 1)
        c2 = ComplexNumber(0, 2)
        result = c1 * c2
        expected = ComplexNumber(-2, 0)
        assert.are.equal expected, result

      it 'Multiply numbers with real and imaginary part', ->
        c1 = ComplexNumber(1, 2)
        c2 = ComplexNumber(3, 4)
        result = c1 * c2
        expected = ComplexNumber(-5, 10)
        assert.are.equal expected, result

    describe 'Division', ->
      it 'Divide purely real numbers', ->
        c1 = ComplexNumber(1, 0)
        c2 = ComplexNumber(2, 0)
        result = c1 / c2
        expected = ComplexNumber(0.5, 0)
        assert.are.equal expected, result

      it 'Divide purely imaginary numbers', ->
        c1 = ComplexNumber(0, 1)
        c2 = ComplexNumber(0, 2)
        result = c1 / c2
        expected = ComplexNumber(0.5, 0)
        assert.are.equal expected, result

      it 'Divide numbers with real and imaginary part', ->
        c1 = ComplexNumber(1, 2)
        c2 = ComplexNumber(3, 4)
        result = c1 / c2
        expected = ComplexNumber(0.44, 0.08)
        assert.are.equal expected, result

  describe 'Absolute value', ->
    it 'Absolute value of a positive purely real number', ->
      c = ComplexNumber(5, 0)
      result = c\abs!
      assert.are.equal 5, result

    it 'Absolute value of a negative purely real number', ->
      c = ComplexNumber(-5, 0)
      result = c\abs!
      assert.are.equal 5, result

    it 'Absolute value of a purely imaginary number with positive imaginary part', ->
      c = ComplexNumber(0, 5)
      result = c\abs!
      assert.are.equal 5, result

    it 'Absolute value of a purely imaginary number with negative imaginary part', ->
      c = ComplexNumber(0, -5)
      result = c\abs!
      assert.are.equal 5, result

    it 'Absolute value of a number with real and imaginary part', ->
      c = ComplexNumber(3, 4)
      result = c\abs!
      assert.are.equal 5, result

  describe 'Complex conjugate', ->
    it 'Conjugate a purely real number', ->
      c = ComplexNumber(5, 0)
      result = c\conjugate!
      expected = ComplexNumber(5, 0)
      assert.are.equal expected, result

    it 'Conjugate a purely imaginary number', ->
      c = ComplexNumber(0, 5)
      result = c\conjugate!
      expected = ComplexNumber(0, -5)
      assert.are.equal expected, result

    it 'Conjugate a number with real and imaginary part', ->
      c = ComplexNumber(1, 1)
      result = c\conjugate!
      expected = ComplexNumber(1, -1)
      assert.are.equal expected, result

  describe 'Complex exponential function', ->
    it "Euler's identity/formula", ->
      c = ComplexNumber(0, pi)
      result = c\exp!
      expected = ComplexNumber(-1, 0)
      assert.are.equal expected, result

    it 'Exponential of 0', ->
      c = ComplexNumber(0, 0)
      result = c\exp!
      expected = ComplexNumber(1, 0)
      assert.are.equal expected, result

    it 'Exponential of a purely real number', ->
      c = ComplexNumber(1, 0)
      result = c\exp!
      expected = ComplexNumber(e, 0)
      assert.are.equal expected, result

    it 'Exponential of a number with real and imaginary part', ->
      c = ComplexNumber(ln(2), pi)
      result = c\exp!
      expected = ComplexNumber(-2, 0)
      assert.are.equal expected, result

    it 'Exponential resulting in a number with real and imaginary part', ->
      c = ComplexNumber(ln(2)/2, pi/4)
      result = c\exp!
      expected = ComplexNumber(1, 1)
      assert.are.equal expected, result

  describe 'Operations between real numbers and complex numbers', ->
    it 'Add real number to complex number', ->
      result = add ComplexNumber(1, 2), 5
      expected = ComplexNumber(6, 2)
      assert.are.equal expected, result

    it 'Add complex number to real number', ->
      result = add 5, ComplexNumber(1, 2)
      expected = ComplexNumber(6, 2)
      assert.are.equal expected, result

    it 'Subtract real number from complex number', ->
      result = sub ComplexNumber(5, 7), 4
      expected = ComplexNumber(1, 7)
      assert.are.equal expected, result

    it 'Subtract complex number from real number', ->
      result = sub 4, ComplexNumber(5, 7)
      expected = ComplexNumber(-1, -7)
      assert.are.equal expected, result

    it 'Multiply complex number by real number', ->
      result = mul ComplexNumber(2, 5), 5
      expected = ComplexNumber(10, 25)
      assert.are.equal expected, result

    it 'Multiply real number by complex number', ->
      result = mul 5, ComplexNumber(2, 5)
      expected = ComplexNumber(10, 25)
      assert.are.equal expected, result

    it 'Divide complex number by real number', ->
      result = div ComplexNumber(10, 100), 10
      expected = ComplexNumber(1, 10)
      assert.are.equal expected, result

    it 'Divide real number by complex number', ->
      result = div 5, ComplexNumber(1, 1)
      expected = ComplexNumber(2.5, -2.5)
      assert.are.equal expected, result
