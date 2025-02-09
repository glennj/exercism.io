module Gigasecond exposing (add)

import Time


add : Time.Posix -> Time.Posix
add time =
    let
        gigasecond = 10^9 * 1000 -- one billion seconds in milliseconds
    in
    time |> Time.posixToMillis |> (+) gigasecond |> Time.millisToPosix
