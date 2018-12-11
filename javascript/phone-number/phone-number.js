/* eslint-disable no-multi-spaces */
export default class PhoneNumber {
  constructor(str) {
    this.num = null;
    const digits = str.replace(/[^0-9]/g, '').replace(/^1/, '');
    if (digits.length === 10
        && !/^[01]/.test(digits)      // area code
        && !/^...[01]/.test(digits)   // exchange
    ) {
      this.num = digits;
    }
  }

  number() {
    return this.num;
  }
}
