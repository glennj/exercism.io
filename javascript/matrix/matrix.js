export class Matrix {
  constructor(str) {
    this.rows = str.split('\n').map(line => line.match(/\d+/g).map(d => parseInt(d, 10)));

    if (!this.rows.every(row => row.length === this.rows[0].length)) {
      throw new Error('rows have unequal lengths');
    }

    this.columns = this.rows[0].map((_, i) => this.rows.map(row => row[i]));
  }
}
