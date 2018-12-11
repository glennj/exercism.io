class ArgumentError extends Error {}

class WordProblem {
  private expr: string = ''

  constructor(question: string) {
    // match from the first number to the last number.
    // This will trim off the leading "What is" and the trailing "?"
    // I assume what remains is the actual arithmetic phrase.
    const m = question.match(/-?\d.+\d/)
    if (m) {
      this.expr = m[0]
        .replace(/\s+plus\s+/g,          ' + ')
        .replace(/\s+minus\s+/g,         ' - ')
        .replace(/\s+divided by\s+/g,    ' / ')
        .replace(/\s+multiplied by\s+/g, ' * ')
    }
  }

  answer(): number {
    const words = this.expr.split(/ /)
    if (words.length < 3 || words.length % 2 === 0) {
      throw new ArgumentError('Cannot find an arithmetic expression')
    }

    // Assuming that we have the valid form:
    //    operand operation [ operand operation ...] operand
    let result = this.toNum(words.shift())
    while (words.length) {
      const operation = words.shift()
      const operand = this.toNum(words.shift())
      switch (operation) {
        case '+': result += operand; break
        case '-': result -= operand; break
        case '*': result *= operand; break
        case '/': result /= operand; break
        default:  throw new ArgumentError('Unknown operation')
      }
    }
    return result
  }

  private toNum(word: string | undefined): number {
    if (! word) {
      throw new ArgumentError('Operand expected')
    }
    const num = parseInt(word, 10)
    if (Number.isNaN(num)) {
      throw new ArgumentError('Operand expected, found NaN')
    }
    return num
  }
}

export { WordProblem, ArgumentError }
