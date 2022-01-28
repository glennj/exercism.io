enum Type {
  NUM = 'number',
  OP = 'operation'
}

type CalcFunction = (a: number, b: number) => number

type Token = {
  type:   Type,
  value?: number,
  fn?:    CalcFunction,
}

class QuestionLexer {
  private words: string[]

  constructor(question: string) {
    this.words = question
      .trim()
      .toLowerCase()
      .replace(/\?$/, '')
      .split(/\s+/)

    const prefix = `${this.words.shift()} ${this.words.shift()}`
    if (prefix !== "what is")
      throw new Error('Unknown operation')
  }

  next(): Token|undefined {
    let word = this.words.shift()
    if (word === undefined)
      return

    const num = parseInt(word, 10)
    if (!isNaN(num))
      return {type: Type.NUM, value: num}

    if (['multiplied', 'divided'].includes(word))
      word += this.words.shift()

    switch (word) {
      case 'plus':
        return {type: Type.OP, fn: (a, b) => a + b}
        break
      case 'minus':
        return {type: Type.OP, fn: (a, b) => a - b}
        break
      case 'multipliedby':
        return {type: Type.OP, fn: (a, b) => a * b}
        break
      case 'dividedby':
        return {type: Type.OP, fn: (a, b) => a / b}
        break
      default:
        throw new Error('Unknown operation')
        break
    }
  }
}

export const answer = (question: string): number => {
  const tokens = new QuestionLexer(question)

  let expected: Type = Type.NUM
  let currentOp: CalcFunction = (_, b) => b
  let result: number = -42
  let token: Token|undefined

  while (token = tokens.next()) {
    if (token.type !== expected) 
      throw new Error('Syntax error')

    switch (token.type) {
      case Type.NUM:
        result = currentOp(result, token.value!)
        expected = Type.OP
        break
      case Type.OP:
        currentOp = token.fn!
        expected = Type.NUM
        break
    }
  }

  if (expected === Type.NUM) 
    throw new Error('Syntax error')

  return result
}

