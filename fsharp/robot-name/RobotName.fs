module RobotName

open System.Collections.Generic

type Robot = Robot of name: string

// Using a .NET HashSet which adds/removes elements mutably
let private names = HashSet<string>()

let private rand = System.Random ()
let private randLetter () = char (65 + rand.Next(26))
let private randNumber () = rand.Next(1000)

let rec private newName () =
    let name = sprintf "%c%c%03d" (randLetter()) (randLetter()) (randNumber())
    if names.Add(name) then
        name
    else
        newName()

let mkRobot() = Robot(newName())

let name (Robot(name)) = name

let reset (Robot(oldName)) = 
    let newRobot = mkRobot()
    names.Remove(oldName) |> ignore
    newRobot

(* For further study, martinfreedman's solution
   which uses FSharp.Control.MailboxProcessor.
   That acts like an elixir Agent, and can maintain "global state"

   https://exercism.org/tracks/fsharp/exercises/robot-name/solutions/martinfreedman
*)
