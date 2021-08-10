import "./byte" for Byte
import "./forth-stack" for ForthStack

class Forth {

  construct new() {
    _stack = ForthStack.new()
    _macros = {}
    _builtins = {
      "+":    Fn.new { _stack.add() },
      "-":    Fn.new { _stack.sub() },
      "*":    Fn.new { _stack.mul() },
      "/":    Fn.new { _stack.div() },
      "dup":  Fn.new { _stack.dup() },
      "drop": Fn.new { _stack.drop() },
      "swap": Fn.new { _stack.swap() },
      "over": Fn.new { _stack.over() },
    }
  }

  stack { _stack.toList }

  evaluate(script) {
    var tokens = Byte.downcase(script).split(" ")

    while (!tokens.isEmpty) {
      var token = tokens.removeAt(0)
      var num = Num.fromString(token)

      if (num != null) {
        _stack.push(num)

      } else if (token == ":") {
        recordMacro(tokens)
        tokens = []

      } else if (_macros.containsKey(token)) {
        // insert macro definition into current script
        tokens = _macros[token] + tokens

      } else if (_builtins.containsKey(token)) {
          _builtins[token].call()

      } else {
          Fiber.abort("Unknown command")
      }
    }
  }

  recordMacro(tokens) {
    if (tokens.removeAt(-1) != ";") {
      Fiber.abort("Macro is missing a semicolon")
    }
    if (Num.fromString(tokens[0]) != null) {
      Fiber.abort("Invalid definition")
    }
    _macros[tokens[0]] = tokens[1..-1].reduce([]) {|defn, tok| 
      if (_macros.containsKey(tok)) {
        defn.addAll(_macros[tok])
      } else {
        defn.add(tok)
      }
      return defn
    }
  }
}
