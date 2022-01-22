const Types = {
  NUM: 'number',
  OP: 'operation'
};

class QuestionLexer {
  constructor(question) {
    this.words = question
      .trim()
      .toLowerCase()
      .replace(/\?$/, '')
      .split(/\s+/);

    const prefix = `${this.words.shift()} ${this.words.shift()}`;
    if (prefix !== "what is")
      throw new Error('Unknown operation');
  }

  /* Returns a "token".
   * A token is an object: { type: aType, value: aValue }
   * where aType is an element of the Types "enum"
   * and aValue is a number or a function, depending on the type.
   */
  next() {
    if (this.words.length === 0)
      return;

    let word = this.words.shift();

    const num = parseInt(word, 10);
    if (!isNaN(num))
      return {type: Types.NUM, value: num};

    if (['multiplied', 'divided'].includes(word))
      word += this.words.shift();

    switch (word) {
      case 'plus':
        return {type: Types.OP, value: (a, b) => a + b};
        break;
      case 'minus':
        return {type: Types.OP, value: (a, b) => a - b};
        break;
      case 'multipliedby':
        return {type: Types.OP, value: (a, b) => a * b};
        break;
      case 'dividedby':
        return {type: Types.OP, value: (a, b) => a / b};
        break;
      default:
        throw new Error('Unknown operation');
        break;
    }
  }
}

export const answer = (question) => {
  const tokens = new QuestionLexer(question)

  let expected = Types.NUM;
  let currentOp = (a, b) => b;
  let result, token;

  while (token = tokens.next()) {
    if (token.type !== expected) 
      throw new Error('Syntax error');

    switch (token.type) {
      case Types.NUM:
        result = currentOp(result, token.value);
        expected = Types.OP;
        break;
      case Types.OP:
        currentOp = token.value;
        expected = Types.NUM;
        break;
    }
  }

  if (result === undefined || expected === Types.NUM) 
    throw new Error('Syntax error');

  return result;
};
