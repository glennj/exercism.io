module CollatzConjecture

let steps (number: int): int option =  
    let rec stepper num count =
        match num with
        | n when n < 1     -> None
        | 1                -> Some count
        | n when n % 2 = 1 -> stepper (3 * num + 1) (count + 1)
        | _                -> stepper (num / 2) (count + 1)

    stepper number 0
