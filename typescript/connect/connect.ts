/* With significant gratitude to shybyte
 * https://exercism.io/tracks/typescript/exercises/connect/solutions/2e4550709a0241e09842d418c0361225
 */

type Cell = [number, number]
type ConnectBoard = string[][]

function transpose(board: ConnectBoard): ConnectBoard {
  return board[0].map((_, i) => board.map((row) => row[i]))
}

function deepCopy<T>(obj: T): T {
  return JSON.parse(JSON.stringify(obj))
}

export class Board {
  private board: ConnectBoard

  constructor(input: string[]) {
    this.board = input.map((row) => [...row.replace(/\s+/g, '')])
  }

  winner(): string {
    if (Board.isWinner('O',  deepCopy(this.board))) { return 'O' }
    if (Board.isWinner('X', transpose(this.board))) { return 'X' }
    return ''
  }

  private static isWinner(player: string, board: ConnectBoard): boolean {
    const lastRow = board.length - 1

    // find my players in the top row
    const stack = board[0]
      .map((v, i) => [v, i])
      .filter(([v, _]) => v === player)
      .map(([_, i]) => [0, i] as Cell)

    if (stack.length === 0) { return false }
    if (lastRow      === 0) { return true }

    while (stack.length > 0) {
      const [r, c] = stack.pop() as Cell
      for (const [rr, cc] of Board.neighbours(player, board, r, c)) {
        if (rr === lastRow) {
          return true
        }
        board[r][c] = 'seen'
        stack.push([rr, cc])
      }
    }
    return false
  }

  private static neighbours(
      player: string,
      board: ConnectBoard,
      r: number,
      c: number
  ): Cell[] {
    const height = board.length
        , width = board[0].length
        , neighbours: Cell[] = []

    for (const dr of [-1, 0, 1]) {
      if (r + dr < 0 || r + dr >= height) { continue }
      for (const dc of [-1, 0, 1]) {
        if (c + dc < 0 || c + dc >= width) { continue }
        if (dr === dc) { continue }  // cannot move this way
        if (board[r + dr][c + dc] === player) {
          neighbours.push([r + dr, c + dc])
        }
      }
    }
    return neighbours
  }
}
