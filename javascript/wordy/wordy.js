/* eslint-disable no-multi-spaces, indent */

class ArgumentError extends Error {}

class WordProblem {
  constructor(question) {
    const m = question.match(/-?\d.+\d/);
    if (!m) throw new ArgumentError();

    const expr = m[0].replace(/\s+plus\s+/g,          ' + ')
                     .replace(/\s+minus\s+/g,         ' - ')
                     .replace(/\s+divided by\s+/g,    ' / ')
                     .replace(/\s+multiplied by\s+/g, ' * ');

    if (!/[0-9 */+-]/.test(expr)) throw new ArgumentError();

    this.expr = expr;
  }

  answer() {
    const words = this.expr.split(/ /);
    let result = parseInt(words.shift(), 10);
    while (words.length) {
      const operation = words.shift();
      const operand = parseInt(words.shift(), 10);
      switch (operation) {
        case '+': result += operand; break;
        case '-': result -= operand; break;
        case '*': result *= operand; break;
        case '/': result /= operand; break;
        default:  throw new ArgumentError();
      }
    }
    return result;
  }
}

module.exports = { WordProblem, ArgumentError };
