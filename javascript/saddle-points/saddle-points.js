export const saddlePoints = (rows) => {
  const columns = rows[0].map((_, i) => rows.map(row => row[i]));
  const rowsMax = rows.map(row => Math.max(...row));
  const colsMin = columns.map(col => Math.min(...col));
  const points = [];

  for (let r = 0; r < rows.length; r++)
    for (let c = 0; c < columns.length; c++)
      if (rowsMax[r] === colsMin[c])
        points.push({row: r+1, column: c+1});

  return points;
}
