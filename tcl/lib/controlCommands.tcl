package provide controlCommands 0.1

proc until {test command} {
    uplevel [list while "!($test)" $command]
}

# do {some action} while  {some condition}
# do {some action} until  {some condition}
# do {some action} if     {some condition}
# do {some action} unless {some condition}
#
proc do {body keyword condition} {
    switch -exact -- $keyword {
        if     {}
        while  {uplevel $body}
        unless {
            set keyword "if"
            set condition "!($condition)"
        }
        until  {
            set keyword "while"
            set condition "!($condition)"
            uplevel $body
        }
        default {
            error "unknown keyword \"$keyword\": must be if, unless, while or until"
        }
    }
    $keyword {[uplevel [list expr $condition]]} {
        uplevel $body
    }
}



proc lmapWithIndex {vars list body} {
    lassign $vars idxVar elemVar
    upvar 1 $idxVar idx
    upvar 1 $elemVar elem
    set idx -1
    lmap elem $list {
        incr idx
        uplevel 1 $body
    }
}

proc foreachWithIndex {vars list body} {
    lassign $vars idxVar elemVar
    upvar 1 $idxVar idx
    upvar 1 $elemVar elem
    set len [llength $list]
    for {set idx 0} {$idx < $len} {incr idx} {
        set elem [lindex $list $idx]
        uplevel 1 $body
    }
}

proc withIndex {list} {
    set len [llength $list]
    for {set i 0} {$i < $len} {incr i} {
        lappend result $i [lindex $list $i]
    }
    return $result
}
proc select {elemVar list body} {
    upvar 1 $elemVar elem
    lmap elem $list {
        if {[uplevel 1 $body]} {
            set elem
        } else {
            continue
        }
    }
}

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}


proc foldlCoro {initValue varnames coro body} {
    lassign $varnames accumVarname elemVarname
    upvar 1 $accumVarname acc
    upvar 1 $elemVarname  elem

    set acc $initValue
    while {[set elem [$coro]] ne ""} {
        set acc [uplevel 1 $body]
    }
    return $acc
}

proc foldl {initValue varnames list body} {
    lassign $varnames accumVarname elemVarname
    upvar 1 $accumVarname acc
    upvar 1 $elemVarname  elem

    set acc $initValue
    foreach elem $list {
        set acc [uplevel 1 $body]
    }
    return $acc
}

# usage: inject accumVar initVal var1 list1 ?var2 list2 ...? script
# 
# example:
# inject sum 100 a {1 2 3} b {2 4 6} {expr {$sum + $a*$b}}  ;# => 128
#
proc inject {args} {
    if {[llength $args] < 5 || [llength $args] % 2 != 1} {
        error {wrong # args: should be "inject accumVar initValue var1 list1 ?var2 list2 ...? command"}
    }

    set accumVar [lindex $args 0]
    set initValue [lindex $args 1]
    set iterables [lrange $args 2 end-1]
    set script [lindex $args end]

    upvar 1 $accumVar accum
    foreach {var list} $iterables {
        upvar 1 $var $var
    }

    set accum $initValue
    foreach {*}$iterables {
        set accum [uplevel 1 $script]
    }
    return $accum
}

proc all {elemName list condition} {
    upvar 1 $elemName elem
    foreach elem $list {
        if {![uplevel 1 [list expr $condition]]} {
            return false
        }
    }
    return true
}

proc seq {from to step} {
    assert {$step != 0} "step cannot be zero"
    if {$from == $to} {return $from}
    if {$from < $to} {
        if {$step < 0} {error "infinite sequence"}
        set stop {$i <= $to}
    }
    if {$from > $to} {
        if {$step > 0} {error "infinite sequence"}
        set stop {$i >= $to}
    }
    set sequence [list]
    for {set i $from} $stop {incr i $step} {
        lappend sequence $i
    }
    return $sequence
}

############################################################
# modelled after Ruby's each_cons:
# https://ruby-doc.org/core-2.6.3/Enumerable.html#method-i-each_cons
#     % foreach {a b c} {1 2 3 4 5 6} {puts [list $a $b $c]}
#     1 2 3
#     4 5 6
#     % foreach_cons {a b c} {1 2 3 4 5 6} {puts [list $a $b $c]}
#     1 2 3
#     2 3 4
#     3 4 5
#     4 5 6
#
proc foreach_cons {varnames list script} {
    foreach var $varnames {
        upvar 1 $var $var
    }
    set n [llength $varnames]
    set last [expr {[llength $list] - $n}]
    set j [expr {$n - 1}]
    for {set i 0} {$i <= $last} {incr i; incr j} {
        lassign [lrange $list $i $j] {*}$varnames
        uplevel 1 $script
    }
}


