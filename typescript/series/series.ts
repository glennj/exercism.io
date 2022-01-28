export class Series {
  public readonly digits: number[]

  constructor(num: string) {
    this.digits = [...num].map(Number)
  }

  slices(size: number): number[][] {
    if (this.digits.length === 0)
      throw new Error('series cannot be empty')
    if (size === 0)
      throw new Error('slice length cannot be zero')
    if (size < 0)
      throw new Error('slice length cannot be negative')
    if (size > this.digits.length)
      throw new Error('slice length cannot be greater than series length')

    return Array.from({length: this.digits.length - size + 1}, (_, i) => i)
      .reduce((slices: number[][], i) => {
        slices.push(this.digits.slice(i, i + size))
        return slices
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
