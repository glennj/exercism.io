class Position {
  constructor([x, y]) {
    if (x < 0 || x > 7 || y < 0 || y > 7)
      throw new Error('Queen must be placed on the board');

    this.x = x;
    this.y = y;
  }

  toArray() { return [this.x, this.y]; }
}

export class QueenAttack {
  constructor(positioning) {
    positioning ??= {white: undefined, black: undefined};
    this.w = new Position(positioning.white ?? [7, 3]);
    this.b = new Position(positioning.black ?? [0, 3]);
    if (this.w.x === this.b.x && this.w.y === this.b.y)
      throw new Error('Queens cannot share the same space');
  }

  get white() { return this.w.toArray(); }
  get black() { return this.b.toArray(); }

  toString() {
    const board = Array(8).fill().map(() => Array(8).fill('_'));
    board[this.w.x][this.w.y] = 'W';
    board[this.b.x][this.b.y] = 'B';
    return board.map(row => row.join(' ')).join('\n');
  }

  get canAttack() {
    return (this.w.x === this.b.x
         || this.w.y === this.b.y
         || Math.abs(this.w.x - this.b.x) === Math.abs(this.w.y - this.b.y));
  }
}
