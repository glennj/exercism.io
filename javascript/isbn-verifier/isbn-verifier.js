class ISBN {
  constructor(str) {
    this.chars = str.replace(/-/g, '').split('');
    this.check = this.chars.pop();
  }

  isValid() {
    if (this.chars.length === 9
        && this.chars.every(c => /\d/.test(c))
        && /[\dX]/.test(this.check)
    ) {
      const digits = this.chars.map(d => parseInt(d, 10));
      digits.push(this.check === 'X' ? 10 : parseInt(this.check, 10));
      return digits.reduce((sum, d, i) => sum + d * (10 - i), 0) % 11 === 0;
    }
    return false;
  }
}

module.exports = ISBN;

/* community
 *
 * taking advantage of NaN

      isValid() {
        if (this.chars.length !== 10) return false;
        const check = this.chars.pop();
        this.chars.push(check.replace('X', '10'));
        return this.chars.reduce((sum, d, i) => sum + d * (10 - i), 0) % 11 === 0;
      }


 */
