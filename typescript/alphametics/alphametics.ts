/* A brute force solution.
 * Expect slow performance when the number of distinct letters grows.
 */

/* tslint:disable:no-null-keyword */

type Dict = { [key: string]: number }

class Alphametics {
  private letters: string
  private words: string[]

  constructor(equation: string) {
    this.words = equation.match(/[A-Z]+/g) || []
    if (this.words.length < 2) {
      throw new Error('Does not look like an equation')
    }
    this.letters = [...new Set([...equation.replace(/[^A-Z]/g, '')]).values()].join('')
    if (this.letters.length > 10) {
      throw new Error('Cannot solve in base 10: too many letters')
    }
  }

  solve(): Dict | undefined {
    for (const digitStr of this.partialPermutations()) {
      const map = this.mapDigits(digitStr)
      const operands = this.words.map((word) => this.digitize(word, map))

      // numbers not allowed to start with 0
      if (operands.some((n) => /^0/.test(n))) { continue }
        const answer = Number(operands.pop())
        const sum = operands.map(Number).reduce((a, b) => a + b)
        if (sum === answer) {
          return map
        }
    }
    // no solution found
    return
  }

  // return zero-padded numbers of length `size`, with no repeated digit
  private *partialPermutations(): IterableIterator<string> {
    const size = this.letters.length
    // start with "0123..."
    let n = parseInt(Array.from({length: size}, (_, i) => i).join(''), 10)
    // stop at "9876..."
    const to = Number(Array.from({length: size}, (_, i) => 9 - i).join(''))

    while (n <= to) {
      const s = n.toString().padStart(size, '0')
      if (! s.match(/(.).*\1/)) {
        yield s
      }
      n += 1
    }
  }

  // map a digit to each letter
  private mapDigits(digitStr: string): Dict {
    return [...this.letters].reduce((map, c, i) => {
      map[c] = Number(digitStr[i])
      return map
    }, Object.create(null) as Dict)
  }

  // transliterate a word into a number
  private digitize(word: string, map: Dict): string {
    return [...word].reduce((a, b) => a.concat(map[b].toString()), '')
  }
}

export default Alphametics
