/* eslint-disable no-underscore-dangle */
class List {
  constructor(a = []) {
    this.a = a;
  }

  // \x1f is the ascii "field separator" character
  _toS() {
    return this.a.join('\x1f');
  }

  compare(other) {
    const x = this._toS();
    const y = other._toS();

    if (x === y) return 'EQUAL';
    if (x.indexOf(y) !== -1) return 'SUPERLIST';
    if (y.indexOf(x) !== -1) return 'SUBLIST';
    return 'UNEQUAL';
  }
}

module.exports = List;
