# translating the Ruby reference solution, compact version
#

proc solve {puzzle} {
    set puzzle [string toupper $puzzle]
    set words [regexp -all -inline {\w+} $puzzle]
    set keys [luniq [regexp -all -inline {\w} $puzzle]]
    set first_letters [lmap word $words {string index $word 0}]
    set final_letters [lmap word $words {string index $word end}]

    set uniqlast [luniq $final_letters]
    set uniqfirst [luniq $first_letters]
    set firstnotlast [lminus $uniqfirst $uniqlast]
    set uniqrest [luniq [lminus [lminus $keys $first_letters] $final_letters]]
    set answer_last_letter [lpop final_letters]

    foreach lasts [permutations {0 1 2 3 4 5 6 7 8 9} [llength $uniqlast]] {
        set table [zip $uniqlast $lasts]
        set anyFirstIsZero [any first $uniqfirst {
                [dict exists $table $first] &&
                [dict get $table $first] == 0
        }]
        if {$anyFirstIsZero} {
            continue
        }
        set sum [::tcl::mathop::+ {*}[lmap f $final_letters {dict get $table $f}]]
        if {$sum % 10 != [dict get $table $answer_last_letter]} {
            continue
        }
        set digits1 [lminus {1 2 3 4 5 6 7 8 9} [dict values $table]]

        foreach firsts [permutations $digits1 [llength $firstnotlast]] {
            set table2 [dict merge $table [zip $firstnotlast $firsts]]
            set digits2 [lminus {0 1 2 3 4 5 6 7 8 9} [dict values $table2]]

            foreach rest [permutations $digits2 [llength $uniqrest]] {
                set table3 [dict merge $table2 [zip $uniqrest $rest]]
                set expression [string map $table3 $puzzle]
                # rare case when we want the condition unquoted
                if $expression {
                    return $table3
                }
            }
        }
    }

    return {}
}


############################################################
# Utility functions

# return the unique elements of a list: preserves order
#
proc luniq {list} {
    set uniq [dict create]
    foreach item $list {dict set uniq $item ""}
    return [dict keys $uniq]
}

# return items in the first list but not in the second list
#
proc lminus {first second} {
    set result [dict create]
    foreach item $first {dict set result $item ""}
    foreach item $second {dict unset result $item}
    return [dict keys $result]
}

# remove the last element of a list in-place, and return the value
#
proc lpop {listName} {
    upvar 1 $listName list
    set last [lindex $list end]
    set list [lreplace $list[set list {}] end end]
    return $last
}

# zip two lists together - the result can be used as a dict
#
# e.g.:  zip {A B C} {1 2 3} => A 1 B 2 C 3
#
proc zip {letters digits} {
    set map {}
    foreach l $letters d $digits {
        lappend map $l $d
    }
    return $map
}

# returns true if any element of the list satisfies the condition
#
proc any {varName list condition} {
    upvar 1 $varName elem
    foreach elem $list {
        if {[uplevel 1 [list expr $condition]]} {
            return true
        }
    }
    return false
}

############################################################
# Generate permutations
#
# Taking the subset generation code from
# https://wiki.tcl-lang.org/page/Combinatorial+mathematics+functions
#
# Combining with the permutation code from tcllib ::struct::list
# https://core.tcl-lang.org/tcllib/artifact/561775e809b5919e
#

namespace eval ::struct::list {

    proc permutationsOfSize {list size} {
        if {$size < 0 || $size > [llength $list]} {
            error "size must be between 0 and list length inclusive"
        }
        if {$size == 0} {return [list [list]]}
        set perms {}
        set subset {}
        while {[set subset [nextKsubset list $size $subset]] ne ""} {
            lappend perms {*}[Lpermutations $subset]
        }
        return $perms
    }

    ###################################################
    # Generate subsets of size K

    # nextK
    # Returns next k-subset given a current k-subset in ordinal representation.
    # For example the 126 4-subsets of a set of 9 start with {1 2 3 4} and end
    # with {6 7 8 9}.
    # Values copied to and fro between list and array to avoid lreplace hassle.
    #
    proc nextK {n k {current {}}} {
        if {![llength $current]} {
               for {set i 1} {$i <= $k} {incr i} {
                  set c($i) $i
               }
        } elseif {[llength $current] != $k} {
               error "current combination not a k-subset"
        } else {
                for {set i 1; set j 0} {$i <= $k} {incr i; incr j} {
                   set c($i) [lindex $current $j]
                   if {![string is integer -strict $c($i)] || $c($i) == 0} {
                      error "only ordinal numbers 1..n allowed"
                   }
                }
                set ptr $k
                while {$ptr > 0 && $c($ptr) == [expr $n - $k + $ptr]} {
                   incr ptr -1
                }
                if {$ptr == 0} {
                  return {}
                }
                incr c($ptr)
                for {set i [expr $ptr + 1]} {$i <= $k} {incr i} {
                   set c($i) [expr $c([expr $i - 1]) + 1]
                }
        }
        set cL [list]
        for {set i 1} {$i <= $k} {incr i} {
               lappend cL $c($i)
        }
        return $cL
    }

    #
    # nextKsubset
    # Returns next k-subset given a current k-subset.
    # Translates to ordinal numbers, calls nextK and translates back.
    #
    proc nextKsubset {setListName k {subsetList {}}} {
        upvar $setListName nList
        set ordinalList [list]
        foreach elem $subsetList {
               if {[set ndx [lsearch $nList $elem]] == -1} {
                   error "element in subsetList not in setList: $elem"
               }
               lappend ordinalList [expr $ndx + 1]
        }
        set nextList [nextK [llength $nList] $k $ordinalList]
        set kList [list]
        foreach ndx $nextList {
               lappend kList [lindex $nList [expr $ndx - 1]]
        }
        return $kList
    }

    ###################################################
    # Generate permutations of a list

    # ::struct::list::Lfirstperm --
    #
    #        Returns the lexicographically first permutation of the
    #        specified list.
    #
    # Parameters:
    #        list        The list whose first permutation is sought.
    #
    # Results:
    #        A modified list containing the lexicographically first
    #        permutation of the input.
    #
    # Side effects:
    #       None

    proc Lfirstperm {list} {
        return [lsort $list]
    }

    # ::struct::list::Lnextperm --
    #
    #        Accepts a permutation of a set of elements and returns the
    #        next permutatation in lexicographic sequence.
    #
    # Parameters:
    #        list        The list containing the current permutation.
    #
    # Results:
    #        A modified list containing the lexicographically next
    #        permutation after the input permutation.
    #
    # Side effects:
    #       None

    proc Lnextperm {perm} {
        # Find the smallest subscript j such that we have already visited
        # all permutations beginning with the first j elements.

        set len [expr {[llength $perm] - 1}]

        set j $len
        set ajp1 [lindex $perm $j]
        while { $j > 0 } {
            incr j -1
            set aj [lindex $perm $j]
            if { [string compare $ajp1 $aj] > 0 } {
                set foundj {}
                break
            }
            set ajp1 $aj
        }
        if { ![info exists foundj] } return

        # Find the smallest element greater than the j'th among the elements
        # following aj. Let its index be l, and interchange aj and al.

        set l $len
        while { [string compare $aj [set al [lindex $perm $l]]] >= 0 } {
            incr l -1
        }
        lset perm $j $al
        lset perm $l $aj

        # Reverse a_j+1 ... an

        set k [expr {$j + 1}]
        set l $len
        while { $k < $l } {
            set al [lindex $perm $l]
            lset perm $l [lindex $perm $k]
            lset perm $k $al
            incr k
            incr l -1
        }

        return $perm
    }

    # ::struct::list::Lpermutations --
    #
    #        Returns a list containing all the permutations of the
    #        specified list, in lexicographic order.
    #
    # Parameters:
    #        list        The list whose permutations are sought.
    #
    # Results:
    #        A list of lists, containing all        permutations of the
    #        input.
    #
    # Side effects:
    #       None

    proc Lpermutations {list} {

        if {[llength $list] < 2} {
            return [::list $list]
        }

        set res {}
        set p [Lfirstperm $list]
        while {[llength $p]} {
            lappend res $p
            set p [Lnextperm $p]
        }
        return $res
    }
}
###############################################################

interp alias {} permutations {} ::struct::list::permutationsOfSize
