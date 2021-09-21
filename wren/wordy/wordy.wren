import "./byte" for Byte

var OPERATIONS = {
  "plus":       Fn.new {|a, b| a + b},
  "minus":      Fn.new {|a, b| a - b},
  "multiplied": Fn.new {|a, b| a * b},
  "divided":    Fn.new {|a, b| a / b},
}


class Error {
  raise()  { Fiber.abort(message) }
  toString { message }
}

class SyntaxError is Error {
  message { "Syntax error" }
  construct new() {}
  static raise() { this.new().raise() }
}

class UnknownOperationError is Error {
  message { "Unknown operation" }
  construct new() {}
  static raise() { this.new().raise() }
}


class Wordy {
  static answer(question) { this.new(question).evaluate() }

  construct new(question) {
    // expected start of sentence
    if (!question.startsWith("What is")) {
      UnknownOperationError.raise()
    }
    // not covered in the tests
    if (!question.endsWith("?")) {
      SyntaxError.raise()
    }
    // remove prefix and suffix
    _q = question.skip(7).join()[0...-1].trim()
  }

  evaluate() {
    var lexer = WordyLexer.new(_q)
    var result = lexer.next()
    if (!result) {
      // expression has no initial number
      SyntaxError.raise()
    }
    var valid = true
    var operation

    for (token in lexer) {
      if (token is Fn) {
        operation = token
        valid = false
      } else {
        result = operation.call(result, token)
        valid = true
      }
    }

    if (!valid) SyntaxError.raise()
    return result
  }
}


class WordyLexer {
  construct new(expression) {
    _bytes = Byte.bytes(expression)
    _state = "num"
  }

  iterate(token) { next() || false }
  iteratorValue(token) { token }

  next() {
    var token = nextToken_()
    if (token == null) return

    var num = Num.fromString(token)

    if (_state == "num") {
      if (num == null) {
        // expecting to see a number, didn't get one
        SyntaxError.raise()
      }
      _state = "op"
      return num

    } else if (_state == "op") {
      if (num != null) {
        // not expecting a number, got one
        SyntaxError.raise()
      }
      if (!OPERATIONS.containsKey(token)) {
        // found a word we don't understand
        UnknownOperationError.raise()
      }
      // two word operations
      if (["multiplied", "divided"].contains(token)) {
        if (nextToken_() != "by") UnknownOperationError.raise()
      }
      _state = "num"
      return OPERATIONS[token]
    }
  }

  nextToken_() {
    if (_bytes.isEmpty) return
    // accumulate the next sequence of non-spaces
    var tokenBytes = []
    while (!_bytes.isEmpty && !_bytes[0].isSpace) {
      tokenBytes.add(_bytes.removeAt(0))
    }
    // consume the next sequence of spaces
    while (!_bytes.isEmpty && _bytes[0].isSpace) {
      _bytes.removeAt(0)
    }
    return Byte.toString(tokenBytes)
  }
}

