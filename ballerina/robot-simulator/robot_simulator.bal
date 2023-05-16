public type Position record {
    int x;
    int y;
};

public enum Direction {
    NORTH, EAST, SOUTH, WEST
}

public function newRobot(
        Position position = {"x": 0, "y": 0},
        Direction direction = NORTH
) returns Robot {
    return new Robot(position, direction);
}

public function move(Robot robot, string instructions) returns Robot {
    foreach string:Char instruction in instructions {
        match instruction {
            "R" => {robot.turnRight();}
            "L" => {robot.turnLeft();}
            "A" => {robot.advance();}
        }
    }
    return robot;
}

public class Robot {
    private Position pos;
    private Direction dir;

    public function init(Position position, Direction direction) {
        self.pos = position;
        self.dir = direction;
    }

    public function position() returns Position => self.pos;
    public function direction() returns Direction => self.dir;

    public function turnRight() {
    }

    public function turnLeft() {
    }

    public function advance() {
    }
}
