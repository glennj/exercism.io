class SaveTheCow {
  static guess(word, guesses) {
    var game = this.new(word)
    guesses.each {|letter| game.guess(letter) }
    return game.state
  }

  construct new(word) {
    _word = word
    _state = "Ongoing"
    _remainingFailures = 9
    _good = {}
    _mask = mask
  }

  mask {
    return _word.codePoints
      .map {|cp| String.fromCodePoint(cp) }
      .map {|c| _good.containsKey(c) ? c : "_" }
      .join("")
  }

  state {
    return {
      "state": _state,
      "maskedWord": mask,
      "remainingFailures": _remainingFailures
    }
  }

  guess(letter) {
    if (_state == "Lose") {
      Fiber.abort("cannot guess after the game is lost")
    }
    if (_state == "Win") {
      Fiber.abort("cannot guess after the game is won")
    }

    if (_good.containsKey(letter) || !_word.contains(letter)) {
      _remainingFailures = _remainingFailures - 1
      if (_remainingFailures < 0) {
        _state = "Lose"
        _remainingFailures = 0
      }
    } else {
      _good[letter] = true
      if (!mask.contains("_")) {
        _state = "Win"
      }
    }
  }
}
