import "./stack" for Stack

var BRACKETS = { "{":"}", "(":")", "[":"]" }

class Brackets {
  static isPaired(s) {
    var stack = Stack.new()

    for (char in s) {
      if (BRACKETS.keys.contains(char)) {
        stack.push(char)
        continue
      } 
      if (BRACKETS.values.contains(char)) {
        if (stack.isEmpty || BRACKETS[stack.pop()] != char) {
          return false
        }
      }
    }

    return stack.isEmpty
  }
}
