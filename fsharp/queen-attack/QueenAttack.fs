module QueenAttack

let create (position: int * int) = 
    let onBoard n = Seq.contains n {0 .. 7}
    onBoard (fst position) && onBoard (snd position)

let canAttack (queen1: int * int) (queen2: int * int) = 
    (*
        let x1, y1 = queen1
        let x2, y2 = queen2
        x1 = x2 || y1 = y2 || (abs (x1 - x2)) = (abs (y1 - y2))
    *)
    (*
        match (queen1, queen2) with
        | (x1, _), (x2, _) when x1 = x2 -> true
        | (_, y1), (_, y2) when y1 = y2 -> true
        | (x1, y1), (x2, y2) when abs (x1 - x2) = abs (y1 - y2) -> true
        | _ -> false
    *)
    let (x1, y1), (x2, y2) = (queen1, queen2)
    match (abs (x1 - x2), abs (y1 - y2)) with
    | (0, _) -> true
    | (_, 0) -> true
    | (dx, dy) when dx = dy -> true
    | _ -> false
