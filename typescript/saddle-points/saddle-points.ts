type Point = {row: number, column: number}

class SaddlePoints {
  static saddlePoints(input: number[][]): Point[] {
    const m = new Matrix(input)
    const saddlePoints: Point[] = []

    const rowMax: number[][] = m.rows.map((row) => {
      return this.maxIndices(row)
    })

    const colMin: number[][] = m.columns.map((col) => {
      return this.minIndices(col)
    })

    for (let r = 0; r < m.rows.length; r++) {
      for (let c = 0; c < m.columns.length; c++) {
        if (rowMax[r].includes(c) && colMin[c].includes(r)) {
          saddlePoints.push({row: r, column: c})
        }
      }
    }
    return saddlePoints
  }

  private static maxIndices(list: number[]): number[] {
    return this.findIndices(Math.max, list)
  }

  private static minIndices(list: number[]): number[] {
    return this.findIndices(Math.min, list)
  }

  private static findIndices(
      func: (...num: number[]) => number,
      list: number[]
  ): number[] {
    const minmax = func(...list)
    return list
      .map((val, idx) => [val, idx])
      .filter(([val, _]) => val === minmax)
      .map(([_, idx]) => idx)
  }
}

class Matrix {
  readonly rows: number[][]
  readonly columns: number[][]

  constructor(input: number[][]) {
    this.rows = input
    this.columns = this.rows[0].map((_, i) => {
      return this.rows.map((row) => row[i])
    })
  }
}

export default SaddlePoints
