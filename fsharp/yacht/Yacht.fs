module Yacht

type Category = 
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One = 1
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6

let dieValue = int

let private scoreBy die dice =
    dice
    |> Seq.filter(fun d -> d = die)
    |> Seq.length
    |> (*) (dieValue die)

let private yacht dice =
    match dice |> Seq.countBy id |> Seq.length with
    | 1 -> 50
    | _ -> 0

let private sum dice =
    dice |> Seq.map dieValue |> Seq.sum

let private fullHouse dice =
    let grouped = dice |> Seq.countBy id |> Seq.map snd |> Seq.sort |> Seq.toList
    if Seq.length grouped = 2 && grouped = [2;3] 
        then sum dice
        else 0

let private fourOFAKind dice =
    let grouped = Seq.countBy id dice
    match Seq.tryFind (fun pair -> List.contains (snd pair) [4;5]) grouped with
    | None -> 0
    | Some (die, _) -> 4 * dieValue die

let private straight sequence dice =
    if sequence = (dice |> Seq.sortBy dieValue |> Seq.toList)
        then 30
        else 0

let score category dice = 
    match category with
    | Ones           -> scoreBy Die.One dice
    | Twos           -> scoreBy Die.Two dice
    | Threes         -> scoreBy Die.Three dice
    | Fours          -> scoreBy Die.Four dice
    | Fives          -> scoreBy Die.Five dice
    | Sixes          -> scoreBy Die.Six dice
    | FullHouse      -> fullHouse dice
    | FourOfAKind    -> fourOFAKind dice
    | LittleStraight -> straight [Die.One; Die.Two; Die.Three; Die.Four; Die.Five] dice
    | BigStraight    -> straight [Die.Two; Die.Three; Die.Four; Die.Five; Die.Six] dice
    | Choice         -> sum dice
    | Yacht          -> yacht dice
