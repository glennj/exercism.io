function transpose(rows: string[]): string[] {
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

export default { transpose }

/*
export default class Transpose {
  public static transpose(lines: string[]): string[] {
    const transposed: string[] = []

    for (let i = 0; i < lines.length; i += 1) {
      for (let j = 0; j < lines[i].length; j += 1) {
        if (!(j in transposed)) {
          transposed[j] = ''.padEnd(i, ' ')
        }

        transposed[j] += lines[i][j]
      }
    }

    return transposed
  }
}
*/
