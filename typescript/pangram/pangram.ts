class Pangram {
  private letters: Set<string>

  constructor(input: string) {
    this.letters = new Set( [...input.toLowerCase().replace(/[^a-z]/g, '')] )
  }

  isPangram(): boolean {
    return this.letters.size === 26
  }
}

export default Pangram
