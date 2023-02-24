module AllYourBase

let rebase (digits: int list) (iBase: int) (oBase: int): int list option = 
    if iBase < 2 || oBase < 2 then
        None
    elif List.exists (fun d -> not (0 <= d && d < iBase)) digits then
        None
    else
        let decimal = List.fold (fun sum d -> sum * iBase + d) 0 digits
        let rec toOutputBase (digits: int list) num =
            match num with
            | 0 -> if digits.Length = 0 then [0] else digits
            | n -> toOutputBase ((num % oBase) :: digits) (num / oBase)
        Some (toOutputBase [] decimal)
