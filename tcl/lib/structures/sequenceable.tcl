package provide structures::sequenceable 0.1

# classes that mix in Sequenceable need to have
# - method `Data` returning the items in the collection
# - method `add` that adds items to the collection
#       - must return [self]
#

oo::class create Sequenceable {
    # iteration and functional methods
    method for {varNames body} {
        foreach name $varNames {
            upvar 1 $name $name
        }
        foreach $varNames [my Data] [list uplevel 1 $body]
    }

    method map {varNames body {level 1}} {
        foreach name $varNames {
            upvar $level $name $name
        }
        lmap $varNames [my Data] [list uplevel $level $body]
    }
    # return a new object with mapped values
    method mapObj {varNames body} {
        [[self class] new] add {*}[my map $varNames $body 2]
    }

    method take {n} {lrange [my Data] 0 $n-1}
    method skip {n} {lrange [my Data] $n end}

    # returns a new List
    # example usage: 
    #   set x [List new a b c d]
    #   [$x withIndex] for idxItem {
    #       lassign $idxItem idx item
    #       puts [list $item $idx]
    #   }
    #
    # select items at even indices: this is pretty verbose ...
    #   [[$x withIndex] \
    #     selectList idxItem {
    #       lassign $idxItem idx item
    #       expr {$idx % 2 == 0}
    #     }] \
    #     map idxItem {lindex $idxItem end}
    #
    method withIndex {} {
        set idx -1
        [self class] new {*}[my map item {list [incr idx] $item}]
    }

    method select {varName cond {level 1}} {
        upvar $level $varName item
        lmap item [my Data] {
            if {[uplevel $level $cond]} then {set item} else continue
        }
    }
    # return a List with selected values
    method selectList {varName cond} {
        [self class] new {*}[my select $varName $cond 2]
    }

    method reject {varName cond {level 1}} {
        upvar $level $varName item
        lmap item [my Data] {
            if {[uplevel $level $cond]} then continue else {set item}
        }
    }
    # return a List with rejected values
    method rejectList {varName cond} {
        [self class] new {*}[my reject $varName $cond 2]
    }

    # example:
    # set x [List new 1 2 3 4 5]
    # set factorial [$x reduce 1 {fact num} {expr {$fact * $num}}]
    #
    method reduce {initVal varNames body} {
        upvar 1 [lindex $varNames 0] acc
        upvar 1 [lindex $varNames 1] item
        set acc $initVal
        foreach item [my Data] {
            set acc [uplevel 1 $body]
        }
        return $acc
    }

    # use the list's first element as the initial value
    method reduce0 {varNames body} {
        upvar 1 [lindex $varNames 0] acc
        upvar 1 [lindex $varNames 1] item
        set acc [lindex [my Data] 0]
        foreach item [lrange [my Data] 1 end] {
            set acc [uplevel 1 $body]
        }
        return $acc
    }

    #
    method contains {item} {
        expr {$item in [my Data]}
    }
    method containsAll {items} {
        set asSet {}
        foreach item [my Data] {
            dict set asSet $item 1
        }
        foreach item $items {
            if {![dict exists $asSet $item]} then {return false}
        }
        return true
    }

    method all {varName cond {level 1}} {
        upvar $level $varName item
        foreach item [my Data] {
            if {![uplevel $level [list expr $cond]]} then {
                return false
            }
        }
        return true
    }

    method none {varName cond {level 1}} {
        my all $varName "!($cond)" [expr {$level + 1}]
    }

    method any {varName cond} {
        expr {![my none $varName $cond 2]}
    }
}
