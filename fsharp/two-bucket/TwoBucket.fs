module TwoBucket

type Bucket = One | Two
type Pail   = {Id: Bucket; Size: int; Amount: int}
type Result = {Moves: int; GoalBucket: Bucket; OtherBucket: int}

(* Pail operations *)
let private isEmpty pail = pail.Amount = 0
let private isFull pail = pail.Amount = pail.Size
let private empty pail = { pail with Amount = 0 }
let private fill pail = { pail with Amount = pail.Size }
let private pourInto recipient donor =
    let quantity = min donor.Amount (recipient.Size - recipient.Amount)
    ( {donor with Amount = donor.Amount - quantity},
      {recipient with Amount = recipient.Amount + quantity} )


let private solve first second goal =
    let gameOver moves winner loser =
        {Moves = moves; GoalBucket = winner.Id; OtherBucket = loser.Amount}

    let rec solver moves a b =
        // IMO, writing the following as a match expression doesn't add clarity
        if   a.Amount = goal then gameOver moves a b
        elif b.Amount = goal then gameOver moves b a
        elif isEmpty a       then solver (moves + 1) (fill a) b
        elif isFull b        then solver (moves + 1) a (empty b)
        else
            let a, b = a |> pourInto b
            solver (moves + 1) a b

    // first one or two moves, then jump into recursive solver
    if first.Size = goal || second.Size <> goal then
        solver 1 (fill first) second
    else
        // first != goal and second == goal
        solver 2 (fill first) (fill second)


let measure size1 size2 goal start =
    let pailOne = {Id = Bucket.One; Size = size1; Amount = 0}
    let pailTwo = {Id = Bucket.Two; Size = size2; Amount = 0}

    match start with
    | One -> solve pailOne pailTwo goal
    | Two -> solve pailTwo pailOne goal
