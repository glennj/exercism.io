module MazeMaker exposing (..)

import Random exposing (Generator)


type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)


type Treasure
    = Gold
    | Diamond
    | Friendship


deadend : Generator Maze
deadend =
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    Random.uniform Gold [Diamond, Friendship]


room : Generator Maze
room =
    treasure |> Random.map Room

-- shamefully copying from other solutions from here down

branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
    |> Random.andThen (\i -> Random.list i mazeGenerator)
    |> Random.map Branch


maze : Generator Maze
maze =
    Random.weighted (60, deadend) [(15, room), (25, branch (Random.lazy (\_ -> maze)))]
    |> Random.andThen identity


mazeOfDepth : Int -> Generator Maze
mazeOfDepth depth =
    if depth == 0 then
        Random.uniform deadend [ room ] |> Random.andThen identity
    else
        branch (Random.lazy (\_ -> mazeOfDepth (depth - 1)))
