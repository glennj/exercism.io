/* eslint-disable  no-multi-spaces, space-in-parens */

const ofSize = (n) => {
  const a = new Array(n).fill().map(() => new Array(n).fill());
  let step = 0;
  let x = 0;
  let y = 0;

  const nextCell = () => {
    const steps = [[1, 0], [0, 1], [-1, 0], [0, -1]];
    let [dx, dy] = steps[step];
    if ( x + dx ===  n || y + dy ===  n
      || x + dx === -1 || y + dy === -1
      || a[y + dy][x + dx] !== undefined
    ) {
      step = (step + 1) % 4;
      [dx, dy] = steps[step];
    }
    return [x + dx, y + dy];
  };

  for (let i = 1; i <= n * n; i += 1) {
    a[y][x] = i;
    [x, y] = nextCell();
  }
  return a;
};

module.exports = { ofSize };
