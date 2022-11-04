# https://stedolan.github.io/jq/manual/v1.6/#Dates

1e9 as $gigasecond
| .moment as $input   # in the catch expr, "." is the error message, so we need to save this value
| $gigasecond +
    try   ($input + "Z"          | fromdateiso8601)   # date and time, add zone
    catch ($input + "T00:00:00Z" | fromdateiso8601)   # just date, add time and zone
| todateiso8601
| rtrimstr("Z")