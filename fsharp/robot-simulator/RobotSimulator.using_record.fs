module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = {Dir: Direction; Pos: Position}

let create direction position = 
    {Dir = direction; Pos = position}


let turnRight robot =
    {robot with Dir = match robot.Dir with
                      | North -> East
                      | East  -> South
                      | South -> West
                      | West  -> North }

let turnLeft robot =
    {robot with Dir = match robot.Dir with
                      | North -> West
                      | East  -> North
                      | South -> East
                      | West  -> South }

let advance robot =
    let x, y = robot.Pos
    {robot with Pos = match robot.Dir with
                      | North -> (x, y + 1)
                      | East  -> (x + 1, y)
                      | South -> (x, y - 1)
                      | West  -> (x - 1, y) }


let move instructions robot =
    let rec mover instrList robot =
        match instrList with
        | [] -> robot
        | 'R'::rest -> mover rest (robot |> turnRight)
        | 'L'::rest -> mover rest (robot |> turnLeft)
        | 'A'::rest -> mover rest (robot |> advance)
        | _ -> failwith "Unknown instruction"

    mover (Seq.toList instructions) robot
