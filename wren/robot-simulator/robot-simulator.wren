var Bearings = {"east": 0, "north": 90, "west": 180, "south": 270}

class Robot {
  construct new() {
    _bearing = Bearings["north"]
    _x = 0
    _y = 0

    _instructions = {
      // Advance
      "A": Fn.new {
        _x = _x + (_bearing * Num.pi/180).cos.round
        _y = _y + (_bearing * Num.pi/180).sin.round
      },
      // turn Right
      "R": Fn.new { _bearing = (_bearing + 270) % 360 },
      // turn Left
      "L": Fn.new { _bearing = (_bearing + 90) % 360 },
    }
  }

  place(location) {
    if (!Bearings.containsKey(location["direction"])) {
      Fiber.abort("Invalid input")
    }
    _bearing = Bearings[location["direction"]]
    _x = location["x"]
    _y = location["y"]
  }

  bearing {
    for (b in Bearings) {
      if (b.value == _bearing) {
        return b.key
      }
    }
  }

  coordinates { [_x, _y] }

  evaluate(script) {
    for (instruction in script) {
      if (!_instructions.containsKey(instruction)) {
        Fiber.abort("Invalid input")
      }
      _instructions[instruction].call()
    }
  }
}
