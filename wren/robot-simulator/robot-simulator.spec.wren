var TurnRight = Fn.new { |robot |
  robot.evaluate("R")
}

var TurnLeft = Fn.new { |robot |
  robot.evaluate("L")
}

var Advance = Fn.new { |robot |
  robot.evaluate("A")
}

import "./robot-simulator" for Robot
import "wren-testie/testie" for Testie, Expect

Testie.test("Robot") { |do, skip|
  do.describe("Create robot") {
    do.test("facing north by default") {
      var robot = Robot.new()
      Expect.value(robot.bearing).toEqual("north")
    }

    do.test("facing east") {
      var robot = Robot.new()
      robot.place({ "direction": "east", "x": 0, "y": 0 })

      Expect.value(robot.bearing).toEqual("east")
    }

    do.test("facing west, at origin") {
      var robot = Robot.new()
      robot.place({ "direction": "west", "x": 0, "y": 0 })

      Expect.value(robot.bearing).toEqual("west")
      Expect.value(robot.coordinates).toEqual([0, 0])
    }

    do.test("at negative position facing south") {
      var robot = Robot.new()
      robot.place({ "direction": "south", "x": -1, "y": -1 })

      Expect.value(robot.bearing).toEqual("south")
      Expect.value(robot.coordinates).toEqual([-1, -1])
    }
  }

  do.describe("Rotating clockwise") {
    do.test("changes north to east") {
      var robot = Robot.new()
      robot.place({ "direction": "north", "x": 0, "y": 0 })

      TurnRight.call(robot)

      Expect.value(robot.bearing).toEqual("east")
      Expect.value(robot.coordinates).toEqual([0, 0])
    }

    do.test("changes east to south") {
      var robot = Robot.new()
      robot.place({ "direction": "east", "x": 0, "y": 0 })

      TurnRight.call(robot)

      Expect.value(robot.bearing).toEqual("south")
      Expect.value(robot.coordinates).toEqual([0, 0])
    }

    do.test("changes south to west") {
      var robot = Robot.new()
      robot.place({ "direction": "south", "x": 0, "y": 0 })

      TurnRight.call(robot)

      Expect.value(robot.bearing).toEqual("west")
      Expect.value(robot.coordinates).toEqual([0, 0])
    }

    do.test("changes west to north") {
      var robot = Robot.new()
      robot.place({ "direction": "west", "x": 0, "y": 0 })

      TurnRight.call(robot)

      Expect.value(robot.bearing).toEqual("north")
      Expect.value(robot.coordinates).toEqual([0, 0])
    }
  }

  do.describe("Rotating counter-clockwise") {
    do.test("changes north to west") {
      var robot = Robot.new()
      robot.place({ "direction": "north", "x": 0, "y": 0 })

      TurnLeft.call(robot)

      Expect.value(robot.bearing).toEqual("west")
      Expect.value(robot.coordinates).toEqual([0, 0])
    }

    do.test("changes west to south") {
      var robot = Robot.new()
      robot.place({ "direction": "west", "x": 0, "y": 0 })

      TurnLeft.call(robot)

      Expect.value(robot.bearing).toEqual("south")
      Expect.value(robot.coordinates).toEqual([0, 0])
    }

    do.test("changes south to east") {
      var robot = Robot.new()
      robot.place({ "direction": "south", "x": 0, "y": 0 })

      TurnLeft.call(robot)

      Expect.value(robot.bearing).toEqual("east")
      Expect.value(robot.coordinates).toEqual([0, 0])
    }

    do.test("changes east to north") {
      var robot = Robot.new()
      robot.place({ "direction": "east", "x": 0, "y": 0 })

      TurnLeft.call(robot)

      Expect.value(robot.bearing).toEqual("north")
      Expect.value(robot.coordinates).toEqual([0, 0])
    }
  }

  do.describe("Moving forward one") {
    do.test("advance when facing north") {
      var robot = Robot.new()
      robot.place({ "direction": "north", "x": 0, "y": 0 })

      Advance.call(robot)

      Expect.value(robot.coordinates).toEqual([0, 1])
      Expect.value(robot.bearing).toEqual("north")
    }

    do.test("advance when facing south") {
      var robot = Robot.new()
      robot.place({ "direction": "south", "x": 0, "y": 0 })

      Advance.call(robot)

      Expect.value(robot.coordinates).toEqual([0, -1])
      Expect.value(robot.bearing).toEqual("south")
    }

    do.test("advance when facing east") {
      var robot = Robot.new()
      robot.place({ "direction": "east", "x": 0, "y": 0 })

      Advance.call(robot)

      Expect.value(robot.coordinates).toEqual([1, 0])
      Expect.value(robot.bearing).toEqual("east")
    }

    do.test("advance when facing west") {
      var robot = Robot.new()
      robot.place({ "direction": "west", "x": 0, "y": 0 })

      Advance.call(robot)

      Expect.value(robot.coordinates).toEqual([-1, 0])
      Expect.value(robot.bearing).toEqual("west")
    }
  }

  do.describe("Follow series of instructions") {
    do.test("moving east and north from README") {
      var robot = Robot.new()
      robot.place({ "x": 7, "y": 3, "direction": "north" })

      robot.evaluate("RAALAL")

      Expect.value(robot.coordinates).toEqual([9, 4])
      Expect.value(robot.bearing).toEqual("west")
    }

    do.test("moving west and north") {
      var robot = Robot.new()
      robot.place({ "x": 0, "y": 0, "direction": "north" })

      robot.evaluate("LAAARALA")

      Expect.value(robot.coordinates).toEqual([-4, 1])
      Expect.value(robot.bearing).toEqual("west")
    }

    do.test("moving west and south") {
      var robot = Robot.new()
      robot.place({ "x": 2, "y": -7, "direction": "east" })

      robot.evaluate("RRAAAAALA")

      Expect.value(robot.coordinates).toEqual([-3, -8])
      Expect.value(robot.bearing).toEqual("south")
    }

    do.test("moving east and north") {
      var robot = Robot.new()
      robot.place({ "x": 8, "y": 4, "direction": "south" })

      robot.evaluate("LAAARRRALLLL")

      Expect.value(robot.coordinates).toEqual([11, 5])
      Expect.value(robot.bearing).toEqual("north")
    }

    do.test("instruct many robots") {
      var robot1 = Robot.new()
      var robot2 = Robot.new()
      var robot3 = Robot.new()
      robot1.place({ "x": 0, "y": 0, "direction": "north" })
      robot2.place({ "x": 2, "y": -7, "direction": "east" })
      robot3.place({ "x": 8, "y": 4, "direction": "south" })

      robot1.evaluate("LAAARALA")
      robot2.evaluate("RRAAAAALA")
      robot3.evaluate("LAAARRRALLLL")

      Expect.value(robot1.coordinates).toEqual([-4, 1])
      Expect.value(robot1.bearing).toEqual("west")

      Expect.value(robot2.coordinates).toEqual([-3, -8])
      Expect.value(robot2.bearing).toEqual("south")

      Expect.value(robot3.coordinates).toEqual([11, 5])
      Expect.value(robot3.bearing).toEqual("north")
    }
  }

  do.describe("Error handling") {
    do.test("invalid robot bearing") {
      var robot = Robot.new()
      Expect.that {
        robot.place({ "direction": "crood", "x": 0, "y": 0 })
      }.abortsWith("Invalid input")
    }

    do.test("invalid instruction") {
      var robot = Robot.new()
      Expect.that {
        robot.evaluate("LAX")
      }.abortsWith("Invalid input")
    }
  }
}
