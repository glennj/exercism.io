type Position = [number, number]

class QueenAttack {
  public readonly white: Position
  public readonly black: Position

  constructor({white, black}: {white: Position, black: Position}) {
    if (white[0] === black[0] && white[1] === black[1]) {
      throw new Error('Queens cannot share the same space')
    }
    this.white = white
    this.black = black
  }

  canAttack(): boolean {
    const dx = Math.abs( this.white[0] - this.black[0] )
    const dy = Math.abs( this.white[1] - this.black[1] )
  }

  toString(): string {
    const board = Array.from({length: 8}, () => new Array(8).fill('_'))
    board[ this.white[0] ][ this.white[1] ] = 'W'
    board[ this.black[0] ][ this.black[1] ] = 'B'
    return board.map((row) => row.join(' ') + '\n').join('')
  }
}

export default QueenAttack
