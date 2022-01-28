export const ofSize = (n: number): number[][] => {
  const matrix: number[][] =
    new Array(n)
      .fill(undefined)
      .map( () => new Array(n).fill(undefined) )

  let step = 0
  let x = 0
  let y = 0

  // function defined here so it can access local variables.
  const nextCell = () => {
    const steps = [[1, 0], [0, 1], [-1, 0], [0, -1]]
    let [dx, dy] = steps[step]
    if (   x + dx ===  n || y + dy ===  n
        || x + dx === -1 || y + dy === -1
        || matrix[y + dy][x + dx] !== undefined
    ) {
      step = (step + 1) % steps.length; // rare case where semicolon is needed
      [dx, dy] = steps[step]
    }
    return [x + dx, y + dy]
  }

  for (let i = 1; i <= n * n; i += 1) {
    matrix[y][x] = i;
    [x, y] = nextCell()
  }

  return matrix
}
