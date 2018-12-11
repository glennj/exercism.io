class NoRowsError extends Error {}
class NoColumnsError extends Error {}

// abstract away the awkward [x, y] => [y][x] indexing
class MinesweeperBoard {
  constructor(lines) {
    if (lines.length === 0) throw new NoRowsError();
    this.rows = lines.map(row => row.split(''));
    this.nrows = this.rows.length;
    this.ncols = this.rows[0].length;
    if (this.ncols === 0) throw new NoColumnsError();
  }

  get(x, y) { return this.rows[y][x]; }

  set(x, y, data) {
    this.rows[y][x] = data;
    return this;
  }

  /* eslint-disable  curly, nonblock-statement-body-position, space-in-parens */
  /* eslint yoda: ["error", "never", { "exceptRange": true }] */

  neighbours(x, y) {
    const list = [];
    for (let xx = x - 1; xx <= x + 1; xx += 1)
      for (let yy = y - 1; yy <= y + 1; yy += 1)
        if ( (0 <= xx && xx < this.ncols)
          && (0 <= yy && yy < this.nrows)
          && (x !== xx || y !== yy)
        )
          list.push(this.get(xx, yy));
    return list;
  }

  // usage: board.eachCell((x, y, data) => ...);
  eachCell(func) {
    for (let x = 0; x < this.ncols; x += 1)
      for (let y = 0; y < this.nrows; y += 1)
        func(x, y, this.get(x, y));
    return this;
  }

  /* eslint-enable */

  clone() { return new MinesweeperBoard([...this.toLines()]); }

  toLines() { return this.rows.map(row => row.join('')); }

  toString() { return this.toLines().join('\n'); }
}

const MINE = '*';

class Minesweeper {
  annotate(lines) {
    try {
      this.board = new MinesweeperBoard(lines);
    } catch (e) {
      if (e instanceof NoRowsError) return [];
      if (e instanceof NoColumnsError) return [''];
      throw e;
    }
    const annotated = this.board.clone();

    this.board.eachCell((x, y, data) => {
      if (data !== MINE) {
        const num = this.board.neighbours(x, y)
          .reduce((c, n) => (n === MINE ? c + 1 : c), 0);
        annotated.set(x, y, num || ' ');
      }
    });

    return annotated.toLines();
  }
}

module.exports = Minesweeper;

/* community
 *
 * a tidy solution

      var Minesweeper = function() {};

      Minesweeper.prototype.annotate = function(field) {
          var b = field.map(l => l.split(''));
          var r = b.length;
          var c = b[0] ? b[0].length : 0;
          var peek = (i, j) => i >= 0 && i < r && j >= 0 && j < c && b[i][j] === '*' ? 1 : 0;
          var d = [];
          for (var i = 0; i < r; i++) {
              var l = '';
              for (var j = 0; j < c; j++) {
                  if (b[i][j] === ' ') {
                      var s =
                          peek(i-1, j-1) + peek(i-1, j) + peek(i-1, j+1) +
                          peek(i,   j-1)                + peek(i,   j+1) +
                          peek(i+1, j-1) + peek(i+1, j) + peek(i+1, j+1);
                      l += s > 0 ? s : ' ';
                  } else l += b[i][j];
              }
              d.push(l);
          }
          return d;
      };

 *
 * and another one
 *
      const NEIGHBOR_OFFSETS = [
        [-1, -1], [-1, 0], [-1, 1],
        [0, -1],           [0, 1],
        [1, -1],  [1, 0],  [1, 1]
      ]

      class Minesweeper {
        constructor(rows) {
          this.rows = rows
        }
        annotate() {
          return this.rows.map((columns, i) =>
            columns.replace(/ /g, (cell, j) =>
              this.neighborMines(i, j) || ' '
            )
          )
        }

        neighborMines(i, j) {
          return this.getNeighbors(i, j).reduce((sum, neighbor) => {
            if (neighbor === '*') return sum + 1
            return sum
          }, 0)
        }

        getNeighbors(i, j) {
          return NEIGHBOR_OFFSETS.map(([x, y]) =>
            this.rows[i + x] && this.rows[i + x][j + y]
          )
        }
      }

 */
