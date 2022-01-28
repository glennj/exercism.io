const Pairs: {[index: string]: string} = {
  '}': '{', ']': '[', ')': '('
}

class Stack<T> {
  private values: T[] = []
  push(value: T): void { this.values.unshift(value) }
  pop(): T | undefined { return this.values.shift() }
  isEmpty(): boolean   { return this.values.length === 0 }
}


export function isPaired(input: string): boolean {
  const stack = new Stack<string>()

  for (const ch of input) {
    if (Object.values(Pairs).includes(ch))   // open bracket
      stack.push(ch)

    else if (ch in Pairs)                    // close bracket
      if (stack.pop() !== Pairs[ch])
        return false
  }

  return stack.isEmpty()
}
