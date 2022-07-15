import "wren-testie/testie" for Testie, Expect
import "./robot-name" for Robot

class RobotNameChecker {
  static isValid(name) {
    return  name.count == 5  &&
            isAlpha(name[0]) &&
            isAlpha(name[1]) &&
            isDigit(name[2]) &&
            isDigit(name[3]) &&
            isDigit(name[4])
  }
  static isAlpha(char) { "ABCDEFGHIJKLMNOPQRSTUVWXYZ".contains(char) }
  static isDigit(char) { "0123456789".contains(char) }
}

var testSuite = Testie.test("Robot Name") { |do, skip|
  do.test("robot has a name") {
    var robot = Robot.new()
    Expect.that(
      RobotNameChecker.isValid(robot.name)
    ).toBe(true)
  }

  skip.test("name sticks") {
    var robot = Robot.new()
    var name = robot.name
    Expect.value(name).toEqual(robot.name)
  }

  skip.test("reset changes name") {
    var robot = Robot.new()
    var name = robot.name
    robot.reset()
    Expect.value(name).toNotEqual(robot.name)
  }

  skip.test("reset before name called does not cause an error") {
    var robot = Robot.new()
    robot.reset()
    Expect.that(
      RobotNameChecker.isValid(robot.name)
    ).toBe(true)
  }

  skip.test("reset multiple times") {
    var robot = Robot.new()
    var names = {}
    for (i in (1..5)) {
      robot.reset()
      names[robot.name] = 1
    }
    Expect.value(names.count).toEqual(5)
  }

  skip.test("different robots have different names") {
    var r1 = Robot.new()
    var r2 = Robot.new()
    Expect.value(r1.name).toNotEqual(r2.name)
  }

  skip.test("generate lots of robots") {
    var iterations = 100000
    var seenNames = {}
    for (i in (1..iterations)) {
      var robot = Robot.new()
      seenNames[robot.name] = 1
    }
    Expect.value(seenNames.count).toEqual(iterations)
  }

  skip.test("generate all robots") {
    Robot.resetNames()
    var iterations = 676000
    var seenNames = {}
    for (i in (1..iterations)) {
      var robot = Robot.new()
      seenNames[robot.name] = 1
    }
    Expect.value(seenNames.count).toEqual(iterations)

    // generate the 676,001st robot
    Expect.that {Robot.new()}.abortsWith("All names consumed")
  }
}
