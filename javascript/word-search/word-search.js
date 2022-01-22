/* eslint-disable  no-extend-native, func-names, object-curly-newline,
 *                 no-restricted-syntax, yoda
 */

String.prototype.reverse = function () {
  return this.split('').reverse().join('');
};

class WordSearch {
  constructor(grid) {
    if (grid.length === 0) throw new Error();
    if (grid[0].length === 0) throw new Error();
    this.rows = grid;
    this.columns = grid[0].split('').map((_, i) => grid.map(line => line[i]).join(''));
    this.dim = { x: this.columns.length, y: this.rows.length };
    this.diag1 = this.buildDiag(this.dim.x, i => (i >= 1 - this.dim.x), -1);
    this.diag2 = this.buildDiag(0, i => (i <= 2 * this.dim.x - 1), +1);
  }
 
  buildDiag(start, end, step) {
    const diag = [];
    for (let i = start; end(i); i += step) {
      let line = '';
      let [x, y] = [i, 0];
      while (y < this.dim.y) {
        line = line.concat(0 <= x && x < this.dim.x ? this.rows[y][x] : ' ');
        [x, y] = [x - step, y + 1];
      }
      diag.push(line);
    }
    return diag;
  }

  find(words) {
    const found = {};
    this.notFound = new Set(words);

    this.searchIn(this.rows).forEach(({ word, index, start, dir }) => {
      found[word] = { start: [index, start], end: [index, start + dir * (word.length - 1)] };
    });

    this.searchIn(this.columns).forEach(({ word, index, start, dir }) => {
      found[word] = { start: [start, index], end: [start + dir * (word.length - 1), index] };
    });

    this.searchIn(this.diag1).forEach(({ word, index, start, dir }) => {
      const startX = start;
      const startY = start + (this.dim.x - index) + 1;
      const endX = startX + dir * (word.length - 1);
      const endY = startY + dir * (word.length - 1);
      found[word] = { start: [startX, startY], end: [endX, endY] };
    });

    this.searchIn(this.diag2).forEach(({ word, index, start, dir }) => {
      const startX = start;
      const startY = index - start + 1;
      const endX = startX + dir * (word.length - 1);
      const endY = startY - dir * (word.length - 1);
      found[word] = { start: [startX, startY], end: [endX, endY] };
    });

    return found;
  }

  searchIn(grid) {
    const found = [];
    grid.forEach((line, j) => {
      this.notFound.forEach((word) => {
        let i = line.indexOf(word);
        if (i >= 0) {
          found.push({ word, index: j + 1, start: i + 1, dir: +1 });
          this.notFound.delete(word);
        } else {
          i = line.indexOf(word.reverse());
          if (i >= 0) {
            found.push({ word, index: j + 1, start: i + word.length, dir: -1 });
            this.notFound.delete(word);
          }
        }
      });
    });
    return found;
  }
}

module.exports = WordSearch;
