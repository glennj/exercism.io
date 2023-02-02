module CarsAssemble

let successRate (speed: int): float =
    (*
    match speed with
    | 0             -> 0.0
    | x when x <= 4 -> 1.0
    | x when x <= 8 -> 0.9
    | 9             -> 0.8
    | 10            -> 0.77
    | _             -> failwith "Whoa! Too fast!"
    *)
    // practicing `if` conditional
    if   speed  = 0 then 0.0
    elif speed <= 4 then 1.0
    elif speed <= 8 then 0.9
    elif speed  = 9 then 0.8
    elif speed  = 10 then 0.77
    else failwith "Whoa! Too fast!"

let carsPerHour = 221

let productionRatePerHour (speed: int): float =
    float (speed * carsPerHour) * successRate speed

let workingItemsPerMinute (speed: int): int =
    int (productionRatePerHour speed / 60.0)
