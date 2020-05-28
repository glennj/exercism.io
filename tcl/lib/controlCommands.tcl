
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

proc all {elemName list condition} {
    upvar 1 $elemName elem
    foreach elem $list {
        if {![uplevel 1 [list expr $condition]]} {
            return false
        }
    }
    return true
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
    for {set i 0; set j [expr {$n - 1}]} \
        {$i <= [llength $list] - $n
        {incr i; incr j} \
        {
            lassign [lrange $list $i $j] {*}$varnames
            uplevel 1 $script
        }
}


