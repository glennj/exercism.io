class Queens {
  constructor(positions) {
    const p = positions || { white: [0, 3], black: [7, 3] };
    // TODO further validations
    this.w = { x: p.white[0], y: p.white[1] };
    this.b = { x: p.black[0], y: p.black[1] };
    if (this.w.x === this.b.x && this.w.y === this.b.y) {
      throw new Error('Queens cannot share the same space');
    }
  }

  get white() { return [this.w.x, this.w.y]; }

  get black() { return [this.b.x, this.b.y]; }

  toString() {
    const board = Array(8).fill().map(() => Array(8).fill('_'));
    board[this.w.x][this.w.y] = 'W';
    board[this.b.x][this.b.y] = 'B';
    return board.map(row => `${row.join(' ')}\n`).join('');
  }

  canAttack() {
    return (this.w.x === this.b.x
         || this.w.y === this.b.y
         || Math.abs(this.w.x - this.b.x) === Math.abs(this.w.y - this.b.y));
  }
}

module.exports = Queens;

/* community
 *
 * a tidy sol'n: `Object assign`; `dx*dy`
 *
      class Q {
        constructor(opt) {
          opt = opt || {white: [0,3], black: [7, 3]};
          Object.assign(this, opt);

          if (this.white[0] === this.black[0] &&
              this.white[1] === this.black[1])
            throw 'Queens cannot share the same space';
        }

        canAttack() {
          let dx = this.black[0] - this.white[0]
          ,   dy = this.black[1] - this.white[1];
          return (!(dx*dy) || Math.abs(dy/dx) === 1)
        }

        toString() {
          let board = [...Array(8)].map(r => Array(8).fill('_'));
          board[this.white[0]][this.white[1]] = 'W';
          board[this.black[0]][this.black[1]] = 'B';
          return board.map(r => r.join(' ')).join('\n') + '\n';
        }
      }

 * another, contructor args

    constructor({white, black} = {white: [0,3], black: [7,3]}) {
    ...
    canAttack() {
        let dx = this.white[0] - this.black[0];
        let dy = this.white[1] - this.black[1];
        return !dx || !dy || dx*dx === dy*dy;
    }

 */
