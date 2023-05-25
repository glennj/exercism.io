public type Position record { int x; int y; };

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
            "R" => { robot.turnRight(); }
            "L" => { robot.turnLeft(); }
            "A" => { robot.advance(); }
        }
    }
    return robot;
}

// The tests are directly accessing the instance members, which isn't great.
// Implementing Robot as a record type would be cleaner.

public class Robot {
    Position position;
    Direction direction;

    public function init(Position position, Direction direction) {
        self.position = position;
        self.direction = direction;
    }

    public function turnRight() {
        match self.direction {
            NORTH => { self.direction = EAST; }
            EAST  => { self.direction = SOUTH; }
            SOUTH => { self.direction = WEST; }
            WEST  => { self.direction = NORTH; }
        }
    }

    public function turnLeft() {
        match self.direction {
            NORTH => { self.direction = WEST; }
            EAST  => { self.direction = NORTH; }
            SOUTH => { self.direction = EAST; }
            WEST  => { self.direction = SOUTH; }
        }
    }

    public function advance() {
        match self.direction {
            NORTH => { self.position = {x: self.position.x,   y: self.position.y+1}; }
            EAST  => { self.position = {x: self.position.x+1, y: self.position.y  }; }
            SOUTH => { self.position = {x: self.position.x,   y: self.position.y-1}; }
            WEST  => { self.position = {x: self.position.x-1, y: self.position.y  }; }
        }
    }
}
