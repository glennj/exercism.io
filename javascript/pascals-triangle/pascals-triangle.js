/* iterative style
function buildTriangle(n) {
  const result = [[1]];
  for (let i = 1; i < n; i += 1) {
    const row = new Array(i + 1);
    const prev = result[i - 1];
    for (let j = 0; j <= i; j += 1) {
      row[j] = (prev[j - 1] || 0) + (prev[j] || 0);
    }
    result.push(row);
  }
  return result;
}
*/

// recursive
function buildTriangle(n) {
  if (n === 1) return [[1]];
  const prevTriangle = buildTriangle(n - 1);
  const prevRow = prevTriangle.pop();
  const row = [1].concat(prevRow.map((v, i) => v + (prevRow[i + 1] || 0)));
  return [...prevTriangle, prevRow, row];
}

class Triangle {
  constructor(n) {
    this.rows = buildTriangle(n);
  }

  get lastRow() {
    return this.rows[this.rows.length - 1];
  }
}

module.exports = Triangle;

/* community
 *
 * here's a terse one

      class Triangle {
        constructor(n) {
          this.rows = [this.lastRow = [1]];

          for(let i=0; i<n-1; i++) {
            this.rows.push(this.lastRow = Array(i+2).fill().map( (_,j) => (this.rows[i][j-1]||0) + (this.rows[i][j]||0) ));
          }
        }
      }
 *
 */
