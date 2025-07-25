const clone = (matrix) => JSON.parse(JSON.stringify(matrix));

export class GameOfLife {
  constructor(matrix) {
    this.matrix = clone(matrix);
    this.nrows = matrix.length;
    if (this.nrows) this.ncols = matrix[0].length;
  }

  tick() {
    const next = clone(this.matrix);
    for (let row = 0; row < this.nrows; row++) {
      for (let col = 0; col < this.ncols; col++) {
        const ns = this.countNeighbours(row, col);
        switch (ns) {
          case 3:  next[row][col] = 1; break;
          case 2:  next[row][col] = this.matrix[row][col]; break;
          default: next[row][col] = 0;
        }
      }
    }
    this.matrix = next;
  }

  state() {
    return this.matrix;
  }

  countNeighbours(row, col) {
    let n = 0;
    for (let rr = row - 1; rr <= row + 1; rr += 1)
      if (0 <= rr && rr < this.ncols)
        for (let cc = col - 1; cc <= col + 1; cc += 1)
          if ((0 <= cc && cc < this.nrows) && (row !== rr || col !== cc))
            n += this.matrix[rr][cc];
    return n;
  }
}
