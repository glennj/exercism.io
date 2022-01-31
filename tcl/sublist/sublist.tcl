# This method is simple, but risky: what if one of the
# lists contains the join character?
#
proc sublist_strings {list1 list2} {
    set str1 [join $list1 \uFFFF]
    set str2 [join $list2 \uFFFF] 

    if {$str1 eq $str2} {
        return "equal"
    }
    if {$str1 eq "" || [string first $list1 $list2] != -1} {
        return "sublist"
    }
    if {$str2 eq "" || [string first $list2 $list1] != -1} {
        return "superlist"
    }
    return "unequal"
}


proc sublist_iterative {list1 list2} {

    set func {{shorter longer result} {
        set shortLen [llength $shorter]
        set longLen  [llength $longer]

        for {set i 0} {$i <= $longLen - $shortLen} {incr i} {
            set found true
            for {set j 0} {$j < $shortLen} {incr j} {
                if {[lindex $shorter $j] != [lindex $longer $i+$j]} {
                    set found false
                    break
                }
            }
            if {$found} {
                return $result
            }
        }
        return "unequal"
    }}

    set len1 [llength $list1]
    set len2 [llength $list2]

    if {$len1 == $len2} {
        return [apply $func $list1 $list2 "equal"]
    } elseif {$len1 < $len2} {
        return [apply $func $list1 $list2 "sublist"]
    } else {
        return [apply $func $list2 $list1 "superlist"]
    }
}


interp alias "" sublist "" sublist_iterative
