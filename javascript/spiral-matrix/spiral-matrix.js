/* eslint-disable  no-multi-spaces, space-in-parens */

export const spiralMatrix = (n) => {
  // initialize the matrix: cell values are undefined
  const m = new Array(n).fill().map(() => new Array(n).fill());

  // set up the closure function to move to the next empty cell
  let step = 0;
  let x = 0;
  let y = 0;
  const nextCell = () => {
    const steps = [[1, 0], [0, 1], [-1, 0], [0, -1]];
    let [dx, dy] = steps[step];
    if ( x + dx ===  n || y + dy ===  n
      || x + dx === -1 || y + dy === -1
      || m[y + dy][x + dx] !== undefined
    ) {
      step = (step + 1) % steps.length;
      [dx, dy] = steps[step];
    }
    return [x + dx, y + dy];
  };

  // and populate the matrix
  for (let i = 1; i <= n * n; i++) {
    m[y][x] = i;
    [x, y] = nextCell();
  }

  return m;
};
