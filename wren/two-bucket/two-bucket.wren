import "./math" for Math
import "./bucket" for Bucket

var TwoBucketError = "Goal is impossible"

class TwoBucket {
  static measure(data) { new(data).measure() }

  construct new(data) {
    validate(data["goal"], data["bucketOne"], data["bucketTwo"])

    _first = Bucket.new("one", data["bucketOne"])
    _second = Bucket.new("two", data["bucketTwo"])
    _goal = data["goal"]

    if (data["startBucket"] == "two") {
      var tmp = _first
      _first = _second
      _second = tmp
    }
  }

  validate(goal, b1, b2) {
    if (goal > b1.max(b2)) Fiber.abort(TwoBucketError)
    var gcd = Math.gcd(b1, b2)
    if (!(gcd == 1 || goal % gcd == 0)) Fiber.abort(TwoBucketError)
  }

  measure() {
    var moves = 0

    // reset in case we're run this before
    _first.empty()
    _second.empty()

    // start
    _first.fill()
    moves = moves + 1

    if (_second.size == _goal) {
      _second.fill()
      moves = moves + 1
    }

    while (true) {
      if (_first.amount == _goal) {
        return result(_first, _second, moves)
      }
      if (_second.amount == _goal) {
        return result(_second, _first, moves)
      }

      if (_first.isEmpty) {
        _first.fill()
      } else if (_second.isFull) {
        _second.empty()
      } else {
        _first.pourInto(_second)
      }
      moves = moves + 1
    }
  }

  result(a, b, moves) {
    return {
      "goalBucket": a.name,
      "otherBucket": b.amount,
      "moves": moves
    }
  }
}
