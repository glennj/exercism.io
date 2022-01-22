/* eslint-disable  no-multi-spaces, lines-between-class-members */

export class Crypto {
  constructor(plaintext) {
    this.text = plaintext;
  }

  get ciphertext() {
    const normalized = this.text
      .replace(/[\W_]/g, '')
      .toLowerCase();

    if (normalized.length === 0)
      return '';

    const size = Math.ceil(Math.sqrt(normalized.length));
    const rows = normalized
      .match(new RegExp(`.{1,${size}}`, 'g'))
      .map(row => row.padEnd(size));

    const columns = rows[0].split('')
      .map((_, i) => rows.map(row => row[i]).join(''));

    return columns.join(' ');
  }
}
