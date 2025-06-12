module BirdCount exposing (busyDays, hasDayWithoutBirds, incrementDayCount, today, total)


today : List Int -> Maybe Int
today counts =
    case counts of
        [] -> Nothing
        count :: _ -> Just count


incrementDayCount : List Int -> List Int
incrementDayCount counts =
    case counts of
        [] ->[1]
        count :: cs -> count + 1 :: cs


hasDayWithoutBirds : List Int -> Bool
hasDayWithoutBirds counts =
    case counts of
        [] -> False
        count :: cs ->
            if count == 0 then
                True
            else
                hasDayWithoutBirds cs


total : List Int -> Int
total counts =
    case counts of
        [] -> 0
        count :: cs -> count + total cs


busyDays : List Int -> Int
busyDays counts =
    case counts of
        [] -> 0
        count :: cs -> (if count >= 5 then 1 else 0) + busyDays cs
