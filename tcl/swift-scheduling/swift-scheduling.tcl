oo::class create DeliveryDate {
    variable timeVal
    variable year
    variable month
    variable day
    variable hour

    constructor {timestamp} {
        set timeVal [clock scan $timestamp -format {%Y-%m-%dT%T}]
        lassign [clock format $timeVal -format {%Y %N %e %k}] year month day hour
    }

    method dueDate {target} {
        set dueDate [
            switch -glob -- $target {
                NOW     {my Now}
                ASAP    {my Asap}
                EOW     {my Eow}
                *M      {my Month [scan $target {%dM}]}
                Q*      {my Quarter [scan $target {Q%d}]}
                default {error "Unknown target: $target"}
            }
        ]
        clock format $dueDate -format {%Y-%m-%dT%T}
    }

    # unexported methods

    method MidnightOn {y m d} {
        clock scan "$y $m $d" -format {%Y %N %e}
    }

    method Midnight {} {
        my MidnightOn $year $month $day
    }

    method Now {} {
        clock add $timeVal 2 hours
    }

    method Asap {} {
        if {$hour < 13} {
            clock add [my Midnight] 17 hours
        } else {
            clock add [my Midnight] 1 days 13 hours
        }
    }

    method Eow {} {
        set dow [clock format $timeVal -format {%u}]
        if {$dow in {1 2 3}} {
            # jump ahead to Friday
            set days [expr {5 - $dow}]
            set hour 17
        } else {
            # jump ahead to Sunday
            set days [expr {7 - $dow}]
            set hour 20
        }
        clock add [my Midnight] $days days $hour hours
    }

    method Month {targetMonth} {
        if {$targetMonth < 1 || $targetMonth > 12} {
            error "Invalid month: $targetMonth"
        }

        set y $year
        if {$month >= $targetMonth} then {incr y}

        set dueDate [my MidnightOn $y $targetMonth 1]

        set dow [clock format $dueDate -format {%u}]
        if {$dow in {6 7}} {
            # jump forward to Monday
            set dueDate [clock add $dueDate [expr {8 - $dow}] days]
        }
        clock add $dueDate 8 hours
    }

    method Quarter {targetQuarter} {
        if {$targetQuarter < 1 || $targetQuarter > 4} {
            error "Invalid quarter: $targetQuarter"
        }

        set y $year
        set targetMonth [expr {3 * $targetQuarter}]
        if {$month > $targetMonth} then {incr y}

        # last day of the target month
        set dueDate [clock add [my MidnightOn $y $targetMonth 1] 1 month -1 day]

        set dow [clock format $dueDate -format {%u}]
        if {$dow in {6 7}} {
            # jump back to Friday
            set dueDate [clock add $dueDate [expr {5 - $dow}] days]
        }
        clock add $dueDate 8 hours
    }
}

############################################################
proc deliveryDate {timestamp target} {
    [DeliveryDate new $timestamp] dueDate $target
}
