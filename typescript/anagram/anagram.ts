class Anagram {
  readonly word: string
  private readonly key: string

  constructor(word: string) {
    this.word = word
    this.key = this.toKey(word)
  }

  private toKey(word: string): string {
    return [...word.toLowerCase()].sort().join('')
  }

  matches(...words: string[]): string[] {
    return words
      .filter((word) => word.toLowerCase() !== this.word.toLowerCase())
      .filter((word) => this.toKey(word) === this.key)
  }
}

export { Anagram }
