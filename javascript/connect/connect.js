/* eslint-disable  no-restricted-syntax, yoda, space-in-parens, no-multi-spaces */
/* eslint no-unused-vars: ["error", { "argsIgnorePattern": "^_$" }] */

const deepCopy = obj => JSON.parse(JSON.stringify(obj));

const transpose = m => m[0].map((_, i) => m.map(row => row[i]));

const neighbours = (player, board, r, c) => {
  const height = board.length;
  const width = board[0].length;
  const result = [];

  for (const dr of [-1, 0, 1]) {
    for (const dc of [-1, 0, 1]) {
      const rr = r + dr;
      const cc = c + dc;
      if (   0 <= rr && rr < height
          && 0 <= cc && cc < width
          && dr !== dc // cannot move this way
          && board[rr][cc] === player
      ) {
        result.push([rr, cc]);
      }
    }
  }
  return result;
};

const isWinner = (player, board) => {
  const b = deepCopy(board);
  const lastRow = b.length - 1;

  // find my players in the top row
  const stack = b[0].reduce((me, v, i) => me.concat(v === player ? [[0, i]] : []), []);

  if (stack.length === 0) return false;
  if (lastRow === 0) return true;

  while (stack.length > 0) {
    const [r, c] = stack.pop();
    for (const [rr, cc] of neighbours(player, b, r, c)) {
      if (rr === lastRow) {
        return true;
      }
      b[r][c] = 'seen';
      stack.push([rr, cc]);
    }
  }
  return false;
};


export class Board {
  constructor(input) {
    this.board = input.map(row => [...row.replace(/\s+/g, '')]);
  }

  winner() {
    if (isWinner('O', this.board)) return 'O';
    if (isWinner('X', transpose(this.board))) return 'X';
    return '';
  }
}
