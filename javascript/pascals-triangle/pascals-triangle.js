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
  if (n < 1) return [];
  if (n === 1) return [[1]];
  const prevTriangle = buildTriangle(n - 1);
  const prevRow = prevTriangle.pop();
  const row = [1].concat(prevRow.map((v, i) => v + (prevRow[i + 1] || 0)));
  return [...prevTriangle, prevRow, row];
}

module.exports = {rows: buildTriangle};
