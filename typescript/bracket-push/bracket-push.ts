class BracketPush {
  text: string
  constructor(input: string) {this.text = input}

  isPaired(): boolean {
    const pairs: {[index: string]: string} = {
      '}': '{', ']': '[', ')': '('
    }
    const stack = new Stack<string>()

    for (const char of this.text) {
      if (Object.values(pairs).includes(char)) {
        // open bracket
        stack.push(char)
      }
      else if (char in pairs) {
        // close bracket
        if (stack.peek() !== pairs[char]) {
          return false
        }
        stack.pop()
      }
    }

    return stack.size() === 0
  }
}

class Stack<T> {
  values: T[] = []
  push(value: T): void  { this.values.unshift(value) }
  pop():  T | undefined { return this.values.shift() }
  peek(): T | undefined { return this.values[0] }
  size(): number        { return this.values.length }
}

export default BracketPush
