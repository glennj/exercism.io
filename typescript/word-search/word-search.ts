/* some type aliases */

type Position = [number, number]
type Direction = [-1|0|1, -1|0|1]

type WordPosition = {
  start: Position
  end: Position
}

type ResultObj = {
  [key: string]: WordPosition | undefined
}


/* The word searcher */

export class WordSearch {
  private readonly grid: string[]
  private words: string[] = []
  private result: ResultObj = {}

  constructor(grid: string[]) {
    this.grid = grid
  }

  public find(words: string[]): ResultObj {
    // sorted by length decreasing, so that "javascript" is sought before "java"
    this.words = words.sort((a, b) => b.length - a.length)

    this.result = {}
    words.forEach(word => this.result[word] = undefined)

    eachPosition: {
      for (let row = 0; row < this.grid.length; row++) {
        for (let col = 0; col < this.grid[0].length; col++) {
          this.findAt([row, col])

          if (this.everyWordFound()) {
            break eachPosition
          }
        }
      }
    }

    return this.result
  }

  private isWordFound(word: string): boolean {
    return this.result[word] !== undefined
  }

  private everyWordFound(): boolean {
    return this.words.every(word => this.isWordFound(word))
  }


  private findAt(pos: Position): void {
    const letter = this.grid[pos[0]][pos[1]]
    if (!this.words.some(word => word.startsWith(letter))) {
      return
    }

    const directions: Direction[] = [
      [ 0,  1], // left to right
      [ 1,  1], // top left to bottom right
      [ 1,  0], // top to bottom
      [ 1, -1], // top right to bottom left
      [ 0, -1], // right to left
      [-1, -1], // bottom right to top left
      [-1,  0], // bottom to top
      [-1,  1], // bottom left to top right
    ]
    directions.forEach(dir => this.findDirectionallyAt(pos, dir))
  }

  private findDirectionallyAt(pos: Position, dir: Direction): void {
    const segment = this.segment(pos, dir)

    for (let i = 0; i < this.words.length; i++) {
      const word = this.words[i]

      if (this.isWordFound(word)) {
        continue
      }

      if (segment.startsWith(word)) {
        this.result[word] = {
          start: [
            pos[0] + 1,
            pos[1] + 1
          ],
          end: [
            pos[0] + 1 + (word.length - 1) * dir[0],
            pos[1] + 1 + (word.length - 1) * dir[1],
          ]
        }
        break
      }
    }
  }

  private segment([row, col]: Position, [dr, dc]: Direction): string {
    const chars: string[] = []
    while (
        0 <= row && row < this.grid.length &&
        0 <= col && col < this.grid[0].length
    ) {
      chars.push(this.grid[row][col])
      row += dr
      col += dc
    }
    return chars.join('')
  }
}
