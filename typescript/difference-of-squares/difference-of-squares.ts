class Squares {

  /* these could be "get" functions, but
   * we only need to calculate them once
   */
  readonly squareOfSum:  number
  readonly sumOfSquares: number

  constructor (n: number) {
    let [sum, sumSq] = [0, 0]
    for (let i = 1; i <= n; i++) {
      sum   += i
      sumSq += i * i
    }
    this.squareOfSum  = sum * sum
    this.sumOfSquares = sumSq
  }

  get difference(): number {
    return this.squareOfSum - this.sumOfSquares
  }
}

export default Squares
