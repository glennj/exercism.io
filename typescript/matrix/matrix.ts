export class Matrix {
  readonly rows: number[][]
  readonly columns: number[][]

  constructor(text: string) {
    this.rows = text.split('\n').map((row) => {
      return row.split(/\s+/).map(Number)
    })

    this.columns = this.rows[0].map((_, i) => {
      return this.rows.map((row) => row[i])
    })
  }
}
