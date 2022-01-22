/* eslint-disable  no-multi-spaces, indent */
/* eslint no-underscore-dangle: ["error", { "allowAfterThis": true }] */

export class ComplexNumber {
  constructor(a, b) {
    this.real = a;
    this.imag = b;
  }

  add(other) {
    if (!(other instanceof ComplexNumber)) throw new Error();
    return new ComplexNumber(this.real + other.real, this.imag + other.imag);
  }

  sub(other) {
    if (!(other instanceof ComplexNumber)) throw new Error();
    return new ComplexNumber(this.real - other.real, this.imag - other.imag);
  }

  mul(other) {
    if (!(other instanceof ComplexNumber)) throw new Error();
    const r = this.real * other.real - this.imag * other.imag;
    const i = this.imag * other.real + this.real * other.imag;
    return new ComplexNumber(r, i);
  }

  div(other) {
    if (!(other instanceof ComplexNumber)) throw new Error();
    const denom = ((other.real ** 2) + (other.imag ** 2));
    const r = (this.real * other.real + this.imag * other.imag) / denom;
    const i = (this.imag * other.real - this.real * other.imag) / denom;
    return new ComplexNumber(r, i);
  }

  get abs() {
    if (this._abs === undefined) {
      this._abs = Math.sqrt((this.real ** 2) + (this.imag ** 2));
    }
    return this._abs;
  }

  get conj() {
    if (this._conj === undefined) {
      this._conj = new ComplexNumber(this.real, -this.imag || 0); // avoid "-0"
    }
    return this._conj;
  }

  get exp() {
    if (this._exp === undefined) {
      this._exp = new ComplexNumber(Math.E ** this.real, 0)
                  .mul(new ComplexNumber(Math.cos(this.imag), Math.sin(this.imag)));
    }
    return this._exp;
  }
}
