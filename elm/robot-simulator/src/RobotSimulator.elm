module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { bearing : Bearing
    , coordinates : { x : Int, y : Int }
    }


defaultRobot : Robot
defaultRobot =
    { bearing = North
    , coordinates = { x = 0, y = 0 }
    }


turnRight : Robot -> Robot
turnRight robot =
    case robot.bearing of
        North -> { robot | bearing = East }
        East -> { robot | bearing = South }
        South -> { robot | bearing = West }
        West -> { robot | bearing = North }


turnLeft : Robot -> Robot
turnLeft robot =
    case robot.bearing of
        North -> { robot | bearing = West }
        East -> { robot | bearing = North }
        South -> { robot | bearing = East }
        West -> { robot | bearing = South }


advance : Robot -> Robot
advance robot =
    let { x, y } = robot.coordinates
    in
    case robot.bearing of
        East -> { robot | coordinates = { x = x + 1, y = y } }
        West -> { robot | coordinates = { x = x - 1, y = y } }
        North -> { robot | coordinates = { x = x, y = y + 1 } }
        South -> { robot | coordinates = { x = x, y = y - 1 } }


simulate : String -> Robot -> Robot
simulate directions robot =
    case String.uncons directions of
        Nothing -> robot
        Just ( 'R', rest ) -> turnRight robot |> simulate rest
        Just ( 'L', rest ) -> turnLeft robot |> simulate rest
        Just ( 'A', rest ) -> advance robot |> simulate rest
        Just ( _, rest )   -> robot |> simulate rest
