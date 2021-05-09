
############################################################
#
oo::class create Bucket {
    variable name 
    variable size
    variable amount

    constructor {aName aSize} {
        set name $aName
        set size $aSize
        set amount 0
    }

    method name     {} { return $name }
    method size     {} { return $size }
    method amount   {} { return $amount }
    method capacity {} { expr {$size - $amount} }
    method isFull   {} { expr {$amount == $size} }
    method isEmpty  {} { expr {$amount == 0} }
    method fill     {} { set amount $size }
    method empty    {} { set amount 0 }

    method add {amt} { incr amount $amt }

    method pourInto {other} {
        set amtToPour [expr {min([my amount], [$other capacity])}]
        $other add $amtToPour
        incr amount -$amtToPour
    }
}

############################################################
#
oo::class create TwoBucketGame {
    variable first
    variable second
    variable goal
    variable isDebug

    constructor {input} {
        set params [dict create {*}$input]
        switch -exact -- [dict get $params startBucket] {
            one {
                set first  [Bucket new "one" [dict get $params bucketOne]]
                set second [Bucket new "two" [dict get $params bucketTwo]]
            }
            two {
                set first  [Bucket new "two" [dict get $params bucketTwo]]
                set second [Bucket new "one" [dict get $params bucketOne]]
            }
            default {error "invalid start bucket name"}
        }
        set goal [dict get $params goal]
        set isDebug [expr {[info exists ::env(DEBUG)] && $::env(DEBUG)}]
    }

    method validate {} {
        set gcd [expr {gcd([$first size], [$second size])}]
        assert {$gcd == 1 || $goal % $gcd == 0} "goal unsatisfiable: answer impossible"
        assert {$goal <= max([$first size], [$second size])} "goal too big: answer impossible"
    }

    method solve {} {
        my validate

        $first empty
        $second empty
        set moves 0
        my debug $moves "======= first=[$first size];second=[$second size];goal=$goal"

        $first fill
        incr moves
        my debug $moves fill 

        if {$goal == [$second size]} {
            $second fill
            incr moves
            my debug $moves fill 
        }

        while 1 {
            if {[$first amount] == $goal} {
                return [my output $first $second $moves]
            }
            if {[$second amount] == $goal} {
                return [my output $second $first $moves]
            }

            if {[$first isEmpty]} {
                $first fill
                set dbg_action fill 
            } elseif {[$second isFull]} {
                $second empty
                set dbg_action empty 
            } else {
                $first pourInto $second
                set dbg_action pour 
            }
            incr moves
            my debug $moves $dbg_action
        }
    }

    method debug {move action} {
        if {$isDebug} {
            puts stderr [format {%s - %d. %s (%d, %d)} \
                [clock format [clock seconds] -format {%Y-%m-%d %T}] \
                $move \
                $action \
                [$first amount] \
                [$second amount] \
            ]
        }
    }

    method output {winner loser moves} {
        list "moves" $moves "goalBucket" [$winner name] "otherBucket" [$loser amount]
    }
}

############################################################
# utility functions
#
proc ::tcl::mathfunc::gcd {a b} {
    if {$b == 0} {
        return $a
    }
    set procname [lindex [info level 0] 0]
    tailcall $procname $b [expr {$a % $b}]
}

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

############################################################
#
proc twoBucket {input} {
    [TwoBucketGame new $input] solve
}
