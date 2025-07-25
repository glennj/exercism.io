class NoRowsError extends Error {}
class NoColumnsError extends Error {}

// abstract away the awkward [x, y] => [y][x] indexing
class FlowerField {
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

  // usage: field.eachCell((x, y, data) => ...);
  eachCell(func) {
    for (let x = 0; x < this.ncols; x += 1)
      for (let y = 0; y < this.nrows; y += 1)
        func(x, y, this.get(x, y));
    return this;
  }

  /* eslint-enable */

  clone() { return new FlowerField([...this.toLines()]); }

  toLines() { return this.rows.map(row => row.join('')); }

  toString() { return this.toLines().join('\n'); }
}

const FLOWER = '*';

export const annotate = (lines) => {
  let field;
  try {
    field = new FlowerField(lines);
  } catch (e) {
    if (e instanceof NoRowsError) return [];
    if (e instanceof NoColumnsError) return [''];
    throw e;
  }
  const annotated = field.clone();

  field.eachCell((x, y, data) => {
    if (data !== FLOWER) {
      const num = field.neighbours(x, y)
        .reduce((c, n) => (n === FLOWER ? c + 1 : c), 0);
      annotated.set(x, y, num || ' ');
    }
  });

  return annotated.toLines();
};
