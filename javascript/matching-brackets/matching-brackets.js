const brackets = { '[': ']', '{': '}', '(': ')' };

const isOpenBracket = b => Object.keys(brackets).indexOf(b) > -1;
const isCloseBracket = b => Object.values(brackets).indexOf(b) > -1;

class Stack {
  constructor() { this.stack = []; }
  push(elem)    { this.stack.unshift(elem); return this; }
  pop()         { return this.stack.shift(); }
  peek()        { return this.stack[0]; }
  get length()  { return this.stack.length; }
  isEmpty()     { return this.length === 0; }
}

export const isPaired = (str) => {
  const stack = new Stack();
  for (const c of str) {
    if (isOpenBracket(c)) {
      stack.push(c);
    } else if (isCloseBracket(c)) {
      if (stack.isEmpty()) return false;
      if (c !== brackets[stack.pop()]) return false;
    }
  }
  return stack.isEmpty();
};
