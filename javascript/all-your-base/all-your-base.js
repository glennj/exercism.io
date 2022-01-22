/* eslint  yoda: ["error", "never", { "exceptRange": true }] */
/* eslint-disable  class-methods-use-this, no-multi-spaces */

class Converter {
  validate(digits, from, to) {
    if ((!from) || from <= 1 || !Number.isInteger(from)) {
      throw new Error('Wrong input base');
    }
    if ((!to) || to <= 1 || !Number.isInteger(to)) {
      throw new Error('Wrong output base');
    }
    if (!digits.length) {
      throw new Error('Input has wrong format');
    }
    if (digits[0] === 0 && digits.length > 1) {
      throw new Error('Input has wrong format');
    }
    if (!digits.every(n => (0 <= n && n < from))) {
      throw new Error('Input has wrong format');
    }
  }

  convert(digits, from = null, to = null) {
    this.validate(digits, from, to);
    let decimal = digits.reduce((s, d) => s * from + d, 0);
    const toDigits = [];
    do {
      toDigits.unshift(decimal % to);
      decimal = Math.floor(decimal / to);
    } while (decimal > 0);
    return toDigits;
  }
}

// tests changed since I wrote the class. 
// add a wrapper function
export const convert = (...args) => {
  return new Converter().convert(...args)
};
