class ComplexNumber {
  private _real: number
  private _imag: number
  private _abs:  number
  private _conj: ComplexNumber
  private _exp:  ComplexNumber

  constructor(real: number, imaginary: number) {
    this._real = real
    this._imag = imaginary
  }

  get real(): number { return this._real }
  get imag(): number { return this._imag }

  add(other: ComplexNumber): ComplexNumber {
    return new ComplexNumber(this.real + other.real, this.imag + other.imag)
  }

  sub(other: ComplexNumber): ComplexNumber {
    return new ComplexNumber(this.real - other.real, this.imag - other.imag)
  }

  mul(other: ComplexNumber): ComplexNumber {
    const r = this.real * other.real - this.imag * other.imag
    const i = this.imag * other.real + this.real * other.imag
    return new ComplexNumber(r, i)
  }

  div(other: ComplexNumber): ComplexNumber {
    const denom = ((other.real ** 2) + (other.imag ** 2))
    const r = (this.real * other.real + this.imag * other.imag) / denom
    const i = (this.imag * other.real - this.real * other.imag) / denom
    return new ComplexNumber(r, i)
  }

  get abs(): number {
    if (this._abs === undefined) {
      this._abs = Math.sqrt((this.real ** 2) + (this.imag ** 2))
    }
    return this._abs
  }

  get conj(): ComplexNumber {
    if (this._conj === undefined) {
      this._conj = new ComplexNumber(this.real, -this.imag || 0) // avoid "-0"
    }
    return this._conj
  }

  get exp(): ComplexNumber {
    if (this._exp === undefined) {
      this._exp =
        new ComplexNumber(Math.E ** this.real, 0)
          .mul( new ComplexNumber(Math.cos(this.imag), Math.sin(this.imag)) )
    }
    return this._exp
  }
}

export default ComplexNumber
