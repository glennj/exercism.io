import ballerina/test;

@test:Config
function createRobotAtOriginFacingNorth() {
    Robot robot = newRobot({"x": 0, "y": 0}, NORTH);
    test:assertEquals(robot.position, {"x": 0, "y": 0});
    test:assertEquals(robot.direction, NORTH);
}

@test:Config {
    enable: false
}
function createRobotAtNetativePositionFacingSouth() {
    Robot robot = newRobot({"x": -1, "y": -1}, SOUTH);
    test:assertEquals(robot.position, {"x": -1, "y": -1});
    test:assertEquals(robot.direction, SOUTH);
}

@test:Config {
    enable: false
}
function createRobotWithDefaultAttributes() {
    Robot robot = newRobot();
    test:assertEquals(robot.position, {"x": 0, "y": 0});
    test:assertEquals(robot.direction, NORTH);
}

@test:Config {
    enable: false
}
function rotatingClockwiseChangesNorthToEast() {
    Robot robot = newRobot();
    robot = move(robot, "R");
    test:assertEquals(robot.position, {"x": 0, "y": 0});
    test:assertEquals(robot.direction, EAST);
}

@test:Config {
    enable: false
}
function rotatingClockwiseChangesEastToSouth() {
    Robot robot = newRobot(direction = EAST);
    robot = move(robot, "R");
    test:assertEquals(robot.position, {"x": 0, "y": 0});
    test:assertEquals(robot.direction, SOUTH);
}

@test:Config {
    enable: false
}
function rotatingClockwiseChangesSouthToWest() {
    Robot robot = newRobot(direction = SOUTH);
    robot = move(robot, "R");
    test:assertEquals(robot.position, {"x": 0, "y": 0});
    test:assertEquals(robot.direction, WEST);
}

@test:Config {
    enable: false
}
function rotatingClockwiseChangesWestToNorth() {
    Robot robot = newRobot(direction = WEST);
    robot = move(robot, "R");
    test:assertEquals(robot.direction, NORTH);
}

@test:Config {
    enable: false
}
function rotatingCounterClockwiseChangesNorthToWest() {
    Robot robot = newRobot();
    robot = move(robot, "L");
    test:assertEquals(robot.position, {"x": 0, "y": 0});
    test:assertEquals(robot.direction, WEST);
}

@test:Config {
    enable: false
}
function rotatingCounterClockwiseChangesEastToNorth() {
    Robot robot = newRobot(direction = EAST);
    robot = move(robot, "L");
    test:assertEquals(robot.position, {"x": 0, "y": 0});
    test:assertEquals(robot.direction, NORTH);
}

@test:Config {
    enable: false
}
function rotatingCounterClockwiseChangesSouthToEast() {
    Robot robot = newRobot(direction = SOUTH);
    robot = move(robot, "L");
    test:assertEquals(robot.position, {"x": 0, "y": 0});
    test:assertEquals(robot.direction, EAST);
}

@test:Config {
    enable: false
}
function rotatingCounterClockwiseChangesWestToSouth() {
    Robot robot = newRobot(direction = WEST);
    robot = move(robot, "L");
    test:assertEquals(robot.position, {"x": 0, "y": 0});
    test:assertEquals(robot.direction, SOUTH);
}

@test:Config {
    enable: false
}
function movingForwardFacingNorthIncrementsY() {
    Robot robot = newRobot();
    robot = move(robot, "A");
    test:assertEquals(robot.position, {"x": 0, "y": 1});
    test:assertEquals(robot.direction, NORTH);
}

@test:Config {
    enable: false
}
function movingForwardFacingSouthDecrementsY() {
    Robot robot = newRobot(direction = SOUTH);
    robot = move(robot, "A");
    test:assertEquals(robot.position, {"x": 0, "y": -1});
    test:assertEquals(robot.direction, SOUTH);
}

@test:Config {
    enable: false
}
function movingForwardFacingEastIncrementsX() {
    Robot robot = newRobot(direction = EAST);
    robot = move(robot, "A");
    test:assertEquals(robot.position, {"x": 1, "y": 0});
    test:assertEquals(robot.direction, EAST);
}

@test:Config {
    enable: false
}
function movingForwardFacingWestDecrementsX() {
    Robot robot = newRobot(direction = WEST);
    robot = move(robot, "A");
    test:assertEquals(robot.position, {"x": -1, "y": 0});
    test:assertEquals(robot.direction, WEST);
}

@test:Config {
    enable: false
}
function movingEastAndNorthFromREADME() {
    Robot robot = newRobot({"x": 7, "y": 3}, NORTH);
    robot = move(robot, "RAALAL");
    test:assertEquals(robot.position, {"x": 9, "y": 4});
    test:assertEquals(robot.direction, WEST);
}

@test:Config {
    enable: false
}
function movingWestAndNorth() {
    Robot robot = newRobot({"x": 0, "y": 0}, NORTH);
    robot = move(robot, "LAAARALA");
    test:assertEquals(robot.position, {"x": -4, "y": 1});
    test:assertEquals(robot.direction, WEST);
}

@test:Config {
    enable: false
}
function movingWestAndSouth() {
    Robot robot = newRobot({"x": 2, "y": -7}, EAST);
    robot = move(robot, "RRAAAAALA");
    test:assertEquals(robot.position, {"x": -3, "y": -8});
    test:assertEquals(robot.direction, SOUTH);
}

@test:Config {
    enable: false
}
function movingEastAndNorth() {
    Robot robot = newRobot({"x": 8, "y": 4}, SOUTH);
    robot = move(robot, "LAAARRRALLLL");
    test:assertEquals(robot.position, {"x": 11, "y": 5});
    test:assertEquals(robot.direction, NORTH);
}

@test:Config {
    enable: false
}
function invalidInstructionsAreIgnored() {
    Robot robot = newRobot();
    robot = move(robot, "GOFASTER"); // valid instructions are "A" and "R"
    test:assertEquals(robot.position, {"x": 0, "y": 1});
    test:assertEquals(robot.direction, EAST);
}

