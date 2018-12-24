class Matrix {
  constructor(str) {
    this.rows = str.split('\n').map(line => line.trim().replace(/\s+/g, ' ').split(' ').map(Number));

    if (!this.rows.every(row => row.length === this.rows[0].length)) {
      throw new Error('rows have unequal lengths');
    }

    this.columns = this.rows[0].map((_, i) => this.rows.map(row => row[i]));
  }

  get saddlePoints() {
    const rowsMax = this.rows.map(row => Math.max(...row));
    const colsMin = this.columns.map(col => Math.min(...col));
    const points = [];
    for (let x = 0; x < this.columns.length; x += 1) {
      for (let y = 0; y < this.rows.length; y += 1) {
        if (rowsMax[x] === colsMin[y]) points.push([x, y]);
      }
    }
    return points;
  }
}

module.exports = Matrix;
