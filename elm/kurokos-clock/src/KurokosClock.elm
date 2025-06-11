module KurokosClock exposing (Locale(..), showDateTime, showLocalDate, showLocalTime)

import Time exposing (Month(..), Posix, Zone)


type Locale
    = US
    | JP


showLocalDate : Locale -> Int -> Month -> Int -> String
showLocalDate locale year month day =
    let
        y = String.fromInt year
        d = String.fromInt day
        m = String.fromInt <| monthToInt month
    in
    case locale of
        US -> m ++ "/" ++ d ++ "/" ++ y
        JP -> y ++ "年" ++ m ++ "月" ++ d ++ "日"


showLocalTime : Locale -> Int -> Int -> String
showLocalTime locale hour minute =
    case locale of
        US ->
            let
                h = String.fromInt
                    <|  case hour of
                            0 -> 12
                            _ -> if hour > 12 then hour - 12 else hour

                m = String.fromInt minute |> String.padLeft 2 '0'
                am = if hour < 12 then "AM" else "PM"
            in
            h ++ ":" ++ m ++ " " ++ am

        JP ->
            let
                h = String.fromInt hour
                m = String.fromInt minute
            in
            h ++ "時" ++ m ++ "分"


showDateTime : Locale -> Zone -> Posix -> String
showDateTime locale zone posix =
    let
        year = Time.toYear zone posix
        month = Time.toMonth zone posix
        day = Time.toDay zone posix
        hour = Time.toHour zone posix
        minute = Time.toMinute zone posix

        date = showLocalDate locale year month day
        time = showLocalTime locale hour minute
    in
    case locale of
        US -> date ++ " " ++ time
        JP -> date ++ time


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12
