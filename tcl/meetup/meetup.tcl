# Strategy: starting with the first day of the "nth" section of the month,
# then iterate day-by-day until we find the requested weekDay.
#
# Note the tests use English weekday names, so work in the en_US locale

proc meetup {year month nth weekDay} {
    # TODO catch errors for invalid input

    set firstDay {
        first   1 
        second  8 
        third  15 
        fourth 22 
        last    1
        teenth 13
    }
    set day [clock scan "$year-$month-[dict get $firstDay $nth]" -format {%Y-%m-%d}]

    if {$nth eq "last"} {
        set day [clock add $day 1 month -7 days]
    }

    foreach _ {1 2 3 4 5 6 7} {
        set wday [clock format $day -format {%A} -locale "en_US"]
        if {$wday eq $weekDay} {
            return [clock format $day -format {%Y-%m-%d}]
        }
        set day [clock add $day 1 day]
    }
    error "$nth $weekDay not found in $year-$month"
}
