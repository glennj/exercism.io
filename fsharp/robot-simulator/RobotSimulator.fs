module RobotSimulator

(*
 * Following the "discriminated union" approach to the Robot type. 
 * https://exercism.org/tracks/fsharp/exercises/robot-simulator/approaches/recursion 
 *)

type Direction = North | East | South | West
type Position = int * int
//type Robot = {Dir: Direction; Pos: Position}
type Robot = Robot of direction: Direction * position: Position

let create (direction: Direction) (position: Position): Robot = 
    //{Dir = direction; Pos = position}
    Robot(direction, position)


let private turnRight (Robot(direction, position)) =
    match direction with
    | North -> Robot(East, position)
    | East  -> Robot(South, position)
    | South -> Robot(West, position)
    | West  -> Robot(North, position)

let private turnLeft (Robot(direction, position)) =
    match direction with
    | North -> Robot(West, position)
    | East  -> Robot(North, position)
    | South -> Robot(East, position)
    | West  -> Robot(South, position)

let private advance (Robot(direction, (x, y))) =
    match direction with
    | North -> Robot(direction, (x, y + 1))
    | East  -> Robot(direction, (x + 1, y))
    | South -> Robot(direction, (x, y - 1))
    | West  -> Robot(direction, (x - 1, y))


let move (instructions: string) (robot: Robot): Robot =
    let rec mover instrList robot =
        match instrList with
        | [] -> robot
        | 'R'::rest -> mover rest (robot |> turnRight)
        | 'L'::rest -> mover rest (robot |> turnLeft)
        | 'A'::rest -> mover rest (robot |> advance)
        | _ -> failwith "Unknown instruction"

    mover (Seq.toList instructions) robot
