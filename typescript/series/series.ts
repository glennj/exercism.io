class Series {
  public readonly digits: number[]

  constructor(num: string) {
    this.digits = [...num].map(Number)
  }

  slices(size: number): number[][] {
    if (size > this.digits.length) {
      throw new Error()
    }
    return new Array(this.digits.length + 1 - size)
      .fill(0)
      .map((_, idx) => idx)
      .reduce((slices: number[][], i) => {
        return slices.concat( [ this.digits.slice(i, i + size) ] )
      }, [])

  /* alternate implementation

    const slices = []
    for (let i = 0; i <= this.digits.length - size; i++) {
      slices.push( this.digits.slice(i, i + size) )
    }
    return slices
  */
  }
}

export default Series
