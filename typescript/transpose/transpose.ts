export function transpose(rows: string[]): string[] {
  if (rows.length === 0) {return []}
  // each row must be at least as long as the next one
  for (let i = rows.length - 2; i >= 0; i--) {
    rows[i] = rows[i].padEnd(rows[i + 1].length)
  }
  const matrix = rows.map((r) => r.split(''))
  return matrix[0].map((_, i) => {
    return matrix.map((r) => r[i]).join('')
  })
}
