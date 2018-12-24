class Forth {
  constructor() {
    this.stack = [];
    this.macros = {};
  }

  evaluate(script) {
    const words = script.toLowerCase().replace(/\s+/g, ' ').trim().split(' ');
    while (words.length) {
      const w = words.shift();
      const n = Number(w);

      if (Number.isInteger(n)) this.stack.push(n);
      else if (this.macros[w]) words.unshift(...this.macros[w]);
      else if (w === ':') this.recordMacro(words);
      else if (w === '+') this.do(2, (a, b) => [a + b]);
      else if (w === '-') this.do(2, (a, b) => [a - b]);
      else if (w === '*') this.do(2, (a, b) => [a * b]);
      else if (w === '/') this.div();
      else if (w === 'dup') this.do(1, a => [a, a]);
      else if (w === 'drop') this.do(1, () => {});
      else if (w === 'swap') this.do(2, (a, b) => [b, a]);
      else if (w === 'over') this.do(2, (a, b) => [a, b, a]);
      else throw new Error('Unknown command');
    }
  }

  do(n, f) {
    const result = f(...this.pop(n));
    if (result !== undefined) this.stack.push(...result);
  }

  pop(n) {
    if (this.stack.length < n) throw new Error('Stack empty');
    return this.stack.splice(this.stack.length - n);
  }

  div() {
    this.do(2, (a, b) => {
      if (b === 0) throw new Error('Division by zero');
      return [Math.floor(a / b)];
    });
  }

  recordMacro(words) {
    const macroName = words.shift();
    if (Number.isInteger(Number(macroName))) throw new Error('Invalid definition');
    const idx = words.indexOf(';');
    if (idx === -1) throw new Error('Unterminated macro');
    this.macros[macroName] = words.splice(0, idx);
    words.shift(); // remove the semicolon
  }
}

module.exports = Forth;
