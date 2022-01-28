export function annotate(rows: string[]): string[] {
  return new Minesweeper().annotate(rows)
}

class Minesweeper {
  private board: string[][] = []

  annotate(rows: string[]): string[] {
    this.board = rows.map((row) => row.split(''))
    this.board.forEach((row, r) => {
      row.forEach((_, c) => {
        if (!this.isBomb(r, c)) {
          const num = this.countAdjacentBombs(r, c)
          this.board[r][c] = num === 0 ? ' ' : num.toString()
        }
      })
    })
    return this.board.map((row) => row.join(''))
  }

  private countAdjacentBombs(r: number, c: number): number {
    let count = 0
    for (let rr = r - 1; rr <= r + 1; rr++) {
      for (let cc = c - 1; cc <= c + 1; cc++) {
        if (this.isCell(rr, cc) && this.isBomb(rr, cc)) {
          count += 1
        }
      }
    }
    return count
  }

  private isBomb(r: number, c: number): boolean {
    return this.board[r][c] === '*'
  }

  private isCell(r: number, c: number): boolean {
    return (0 <= r && r < this.board.length)
        && (0 <= c && c < this.board[0].length)
  }
}
