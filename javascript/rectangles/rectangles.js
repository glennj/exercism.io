/* eslint-disable  no-extend-native, func-names, arrow-body-style, no-multi-spaces */
/* eslint no-unused-vars: ["error", { "argsIgnorePattern": "^_$" }] */

/* **** Monkey patch Array to work with [x, y] points nicely **** */
//
// - functions to return just the x's or y's
Array.prototype.x = function () { return this.map(([x, _]) => x); };
Array.prototype.y = function () { return this.map(([_, y]) => y); };
//
// - functions to return just pairs in a row or column
Array.prototype.inRow = function (r) { return this.filter(([_, y]) => y === r); };
Array.prototype.inCol = function (c) { return this.filter(([x, _]) => x === c); };
//
// - function to check inclusion, because [[0,1]].includes([0,1]) return false:
Array.prototype.includesPoint = function ([x, y]) {
  return this.filter(([xx, yy]) => xx === x && yy === y).length !== 0;
};

// - function to return unique ordered pairs (https://stackoverflow.com/a/43241287/7552)
Array.prototype.pairs = function () {
  return [].concat(...this.map((v, i) => this.slice(i + 1).map(w => [v, w])));
};

// get the cartesian coordinates of all the "+" characters
const findVertices = (lines) => {
  const v = [];
  lines.forEach((row, y) => {
    row.split('').forEach((c, x) => {
      if (c === '+') v.push([x, y]);
    });
  });
  return v;
};

const horizontalEdge = new RegExp(/^[+-]+$/); // legal chars for horizontal edge
const verticalEdge = new RegExp(/^[+|]+$/); // legal chars for vertical edge

const complete = (rows, columns, [[x1, y1], [x2, y2]]) => {
  return horizontalEdge.test(rows[y1].slice(x1, x2))    // top edge
      && horizontalEdge.test(rows[y2].slice(x1, x2))    // bottom edge
      && verticalEdge.test(columns[x1].slice(y1, y2))   // left edge
      && verticalEdge.test(columns[x2].slice(y1, y2));  // right edge
};

const findRectangles = (rows) => {
  const columns = Array.from(rows[0]).map((_, i) => rows.map(row => row[i]).join(''));
  const vertices = findVertices(rows);
  const rectangles = [];

  for (let r = 0; r < rows.length; r += 1) {
    vertices.inRow(r).x().pairs().forEach(([x1, x2]) => {
      vertices.inCol(x1).filter(([_, y]) => y > r).y().forEach((y) => {
        if (vertices.includesPoint([x2, y]) && complete(rows, columns, [[x1, r], [x2, y]])) {
          rectangles.push([[x1, r], [x2, y]]);
        }
      });
    });
  }
  return rectangles;
};

const count = (lines) => {
  return lines.length ? findRectangles(lines).length : 0;
};

module.exports = { count };
