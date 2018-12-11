/* eslint-disable  no-multi-spaces, lines-between-class-members */

class Crypto {
  constructor(plaintext) {
    this.normalized = plaintext.replace(/[\W_]/g, '').toLowerCase();
    const size = Math.ceil(Math.sqrt(this.normalized.length));
    this.rows = this.normalized.match(new RegExp(`.{1,${size}}`, 'g'));
    this.cols = this.rows[0].split('')
      .map((_, i) => this.rows.map(row => row[i]).join(''));
  }

  normalizePlaintext() { return this.normalized; }
  size()               { return this.cols.length; }
  plaintextSegments()  { return this.rows; }
  ciphertext()         { return this.cols.join(''); }
}

module.exports = Crypto;
