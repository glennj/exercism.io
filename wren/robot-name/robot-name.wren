import "random" for Random

var LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var DIGITS  = "0123456789"

class Robot {
  static initialize() {
    __rand = Random.new()
    resetNames()
  }

  static resetNames() {
    __names = []
    for (a in LETTERS) {
      for (b in LETTERS) {
        for (c in DIGITS) {
          for (d in DIGITS) {
            for (e in DIGITS) {
              __names.add(a + b + c + d + e)
            }
          }
        }
      }
    }
    __rand.shuffle(__names)
  }

  construct new() { reset() }

  name { _name }

  reset() {
    if (__names.isEmpty) Fiber.abort("All names consumed")
    _name = __names.removeAt(-1)
  }
}

Robot.initialize()
