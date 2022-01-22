/* eslint key-spacing: ["error", { "align": "value" }] */

const isPalindrome = n => String(n) === String(n).split('').reverse().join('');

const EMPTY_RESULT = {value: null, factors: []}

export class Palindromes {
  static generate(options) {
    return new Palindromes(options);
  }

  constructor({minFactor, maxFactor}) {
    this.min = minFactor ?? 1;
    this.max = maxFactor;
  }

  checkLimits() {
    if (this.min > this.max)
      throw new Error('min must be <= max');
  }

  get smallest() {
    this.checkLimits();
    const limit = this.max ** 2;
    for (let p = this.min ** 2; p <= limit; p++) {
      const result = this.isPalindromeProduct(p);
      if (result) return result;
    }
    return EMPTY_RESULT;
  }

  get largest() {
    this.checkLimits();
    const limit = this.min ** 2;
    for (let p = this.max ** 2; p >= limit; p--) {
      const result = this.isPalindromeProduct(p);
      if (result) return result
    }
    return EMPTY_RESULT;
  }

  isPalindromeProduct(n) {
    if (isPalindrome(n)) {
      const fs = this.factors(n);
      if (fs.length > 0)
        return {value: n, factors: fs};
    }
    return;
  }

  factors(n) {
    const pairs = [];
    const limit = Math.min(this.max, Math.sqrt(n));
    for (let i = this.min; i <= limit; i++) {
      if (n % i === 0) {
        const j = n / i;
        if (this.min <= j && j <= this.max)
          pairs.push([i, j]);
      }
    }
    return pairs;
  }
}
