type Position = readonly [number, number]
type Queens = {[key: string]: Position}


class Queen {
  public row: number
  public col: number

  constructor([row, col]: Position) {
    if (row < 0 || row > 7 || col < 0 || col > 7)
      throw new Error('Queen must be placed on the board')

    this.row = row
    this.col = col
  }

  get position(): Position {
    return [this.row, this.col]
  }

  equals(other: Queen) {
    return this.row === other.row && this.col === other.col
  }
}


export class QueenAttack {
  private b: Queen
  private w: Queen

  constructor(queens?: Queens) {
    queens ??= {}
    queens.black ??= [0, 3]
    queens.white ??= [7, 3]

    this.b = new Queen(queens.black)
    this.w = new Queen(queens.white)

    if (this.b.equals(this.w))
      throw new Error('Queens cannot share the same space')
  }

  get black() { return this.b.position; }
  get white() { return this.w.position; }

  get canAttack(): boolean {
    const dx = Math.abs( this.b.row - this.w.row )
    const dy = Math.abs( this.b.col - this.w.col )
    return (dx === 0 || dy === 0 || dx === dy)
  }

  toString(): string {
    const board = Array.from({length: 8}, () => new Array(8).fill('_'))
    board[ this.b.row ][ this.b.col ] = 'B'
    board[ this.w.row ][ this.w.col ] = 'W'
    return board.map((row) => row.join(' ')).join('\n')
  }
}
