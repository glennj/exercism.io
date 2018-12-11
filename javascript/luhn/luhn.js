class Luhn {
  constructor(num) {
    this.valid = false;
    if (/[^\d\s]/.test(num)) return;

    const digits = num.replace(/\D/g, '').split('').reverse();
    if (digits.length <= 1) return;

    const sum = digits.map(d => parseInt(d, 10)).reduce((s, d, i) => {
      if (i % 2) {
        return s + 2 * d - (d < 5 ? 0 : 9);
      }
      return s + d;
    }, 0);

    this.valid = (sum % 10 === 0);
  }
}

module.exports = Luhn;
