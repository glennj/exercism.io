module Meetup exposing (Month(..), Week(..), Weekday(..), meetup)

-- elm install justinmimbs/time-extra

import Time
import Time.Extra


type Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December


type Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


type Week
    = First
    | Second
    | Third
    | Fourth
    | Last
    | Teenth


type alias Calendar =
    List ( Weekday, Time.Posix )


posixMonth : Month -> Time.Month
posixMonth mon =
    case mon of
        January -> Time.Jan
        February -> Time.Feb
        March -> Time.Mar
        April -> Time.Apr
        May -> Time.May
        June -> Time.Jun
        July -> Time.Jul
        August -> Time.Aug
        September -> Time.Sep
        October -> Time.Oct
        November -> Time.Nov
        December -> Time.Dec


getMonth : Time.Posix -> Month
getMonth time =
    case Time.toMonth Time.utc time of
        Time.Jan -> January
        Time.Feb -> February
        Time.Mar -> March
        Time.Apr -> April
        Time.May -> May
        Time.Jun -> June
        Time.Jul -> July
        Time.Aug -> August
        Time.Sep -> September
        Time.Oct -> October
        Time.Nov -> November
        Time.Dec -> December

monthNum : Time.Month -> Int
monthNum month =
    case month of
        Time.Jan -> 1
        Time.Feb -> 2
        Time.Mar -> 3
        Time.Apr -> 4
        Time.May -> 5
        Time.Jun -> 6
        Time.Jul -> 7
        Time.Aug -> 8
        Time.Sep -> 9
        Time.Oct -> 10
        Time.Nov -> 11
        Time.Dec -> 12


getWeekday : Time.Posix -> Weekday
getWeekday time =
    case Time.toWeekday Time.utc time of
        Time.Mon -> Monday
        Time.Tue -> Tuesday
        Time.Wed -> Wednesday
        Time.Thu -> Thursday
        Time.Fri -> Friday
        Time.Sat -> Saturday
        Time.Sun -> Sunday


nextDay : Time.Posix -> Time.Posix
nextDay time =
    time
        |> Time.posixToMillis
        |> (+) (1000 * 60 * 60 * 24) -- one day in milliseconds
        |> Time.millisToPosix


monthCalendar : Month -> Time.Posix -> Calendar -> Calendar
monthCalendar month time acc =
    if month /= getMonth time then
        acc
    else
        monthCalendar
            month
            (nextDay time)
            (acc ++ [ ( getWeekday time, time ) ])


wantedDay : Week -> Weekday -> Int -> Calendar -> Maybe Time.Posix
wantedDay week weekday n calendar =
    case calendar of
        [] ->
            Nothing

        ( wday, time ) :: rest ->
            if wday /= weekday then
                wantedDay week weekday n rest

            else
                case week of
                    First ->
                        if n == 1 then
                            Just time
                        else
                            wantedDay week weekday (n + 1) rest

                    Last ->
                        if n == 1 then -- calendar is reversed for this case
                            Just time
                        else
                            wantedDay week weekday (n + 1) rest

                    Second ->
                        if n == 2 then
                            Just time
                        else
                            wantedDay week weekday (n + 1) rest

                    Third ->
                        if n == 3 then
                            Just time
                        else
                            wantedDay week weekday (n + 1) rest

                    Fourth ->
                        if n == 4 then
                            Just time
                        else
                            wantedDay week weekday (n + 1) rest

                    Teenth ->
                        let
                            day = Time.toDay Time.utc time
                        in
                        if 13 <= day && day <= 19 then
                            Just time
                        else
                            wantedDay week weekday 0 rest


toISO8601Date : Time.Posix -> String
toISO8601Date time =
    let
        toStr n = String.fromInt n |> String.padLeft 2 '0'
    in
    (Time.toYear Time.utc time |> toStr)
        ++ "-"
        ++ (Time.toMonth Time.utc time |> monthNum |> toStr)
        ++ "-"
        ++ (Time.toDay Time.utc time |> toStr)


identity : a -> a
identity x = x


meetup : Int -> Month -> Week -> Weekday -> String
meetup year month week weekday =
    let
        firstDayOfMonth =
            Time.Extra.partsToPosix
                Time.utc
                (Time.Extra.Parts year (posixMonth month) 1 0 0 0 0)

        calendar =
            monthCalendar month firstDayOfMonth []

        cal =
            (if week == Last then List.reverse else identity) calendar

        targetDay =
            wantedDay week weekday 1 cal
    in
    case targetDay of
        Nothing -> "Date not found?!"
        Just time -> toISO8601Date time
