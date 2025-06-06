export function playGame() {
  const aliens = getStartingAliens();
  const rows = aliens.length;
  const columns = aliens[0].length;
  let count = aliens.reduce((n, row) => n + row.filter((a) => a).length, 0);

  let column = 0;
  let direction = 1;

  function move() {
    if (
      (column === 0 && direction === -1) ||
      (column === columns - 1 && direction === 1)
    )
      direction *= -1;

    if (direction === 1) moveRight();
    else moveLeft();

    column += direction;
  }

  function takeTheShot() {
    for (let row = rows - 1; row >= 0; row--) {
      if (aliens[row][column]) {
        shoot();
        --count;
        aliens[row][column] = false;
        return;
      }
    }
  }

  while (count > 0) {
    takeTheShot();
    move();
  }
}
