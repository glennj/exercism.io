class ISBN {
  private readonly isbn: string

  constructor(input: string) {
    this.isbn = input.replace(/-/g, '')
  }

  isValid(): boolean {
    // 9 digits followed by a digit or 'X'
    if (!/^\d{9}[\dX]$/.test(this.isbn)) {
      return false
    }
    const chars = [...this.isbn]
    const check = chars.pop()
    const digits = chars.map(Number)
    digits.push(check === 'X' ? 10 : Number(check))
    const sum = digits.reduce((sum, d, i) => sum + d * (10 - i), 0)
    return sum % 11 === 0
  }
}

export default ISBN
