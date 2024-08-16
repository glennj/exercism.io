import gleam/list
import gleam/string

pub type Robot {
  Robot(direction: Direction, position: Position)
}

pub type Direction {
  North
  East
  South
  West
}

pub type Position {
  Position(x: Int, y: Int)
}

pub fn create(direction: Direction, position: Position) -> Robot {
  Robot(direction, position)
}

pub fn move(
  direction: Direction,
  position: Position,
  instructions: String,
) -> Robot {
  instructions
  |> string.to_graphemes()
  |> list.fold(create(direction, position), fn(robot, instruction) {
    case instruction {
      "R" -> create(turn_right(robot.direction), robot.position)
      "L" -> create(turn_left(robot.direction), robot.position)
      "A" -> create(robot.direction, advance(robot.position, robot.direction))
      _ -> robot
    }
  })
}

fn turn_right(direction: Direction) -> Direction {
  case direction {
    North -> East
    South -> West
    East -> South
    West -> North
  }
}

fn turn_left(direction: Direction) -> Direction {
  case direction {
    North -> West
    South -> East
    East -> North
    West -> South
  }
}

fn advance(position: Position, direction: Direction) -> Position {
  case direction {
    North -> Position(position.x, position.y + 1)
    South -> Position(position.x, position.y - 1)
    East -> Position(position.x + 1, position.y)
    West -> Position(position.x - 1, position.y)
  }
}
