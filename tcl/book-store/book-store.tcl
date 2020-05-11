# Translation of the Java reference implementation:
# https://github.com/exercism/java/blob/master/exercises/book-store/.meta/src/reference/java/BookStore.java

namespace eval BookStore {
    namespace export basketCost
    namespace import ::tcl::mathfunc::min

    variable GROUP_COST {0}
    foreach n {1 2 3 4 5} d {0 5 10 20 25} {
        lappend GROUP_COST [expr {$n * 800 * (100 - $d) / 100}]
    }

    proc basketCost {books} {
        set ordered [sortByGroupSize $books]
        tailcall calculateBasketCost $ordered 0
    }

    proc calculateBasketCost {books priceSoFar} {
        set procname [lindex [info level 0] 0]
        variable GROUP_COST

        if {[llength $books] == 0} {
            return $priceSoFar
        }
        
        set uniqBooks [ldistinct $books]
        set minPrice Inf

        for {set i 1} {$i <= [llength $uniqBooks]} {incr i} {
            set newGroupBooks [lrange $uniqBooks 0 $i-1]
            set remainingBooks $books
            foreach book $newGroupBooks {
                lremoveFirst remainingBooks $book
            }

            set price [$procname $remainingBooks [expr {
                $priceSoFar + [lindex $GROUP_COST $i]
            }]]

            set minPrice [min $minPrice $price]
        }

        return $minPrice
    }

    proc sortByGroupSize {items} {
        set groups {}
        foreach item $items {dict incr groups $item}

        set sortedDict [lsort -int -decr -stride 2 -index end $groups]

        concat {*}[dict values [dict map {item n} $sortedDict {
            lrepeat $n $item
        }]]
    }

    # remove the _first_ instance of an element from a list
    proc lremoveFirst {listVar element} {
        upvar 1 $listVar list
        set idx [lsearch -exact $list $element]
        if {$idx != -1} {
            set list [lreplace [K $list [set list ""]] $idx $idx]
        }
    }

    # ref: https://wiki.tcl-lang.org/page/K
    # particularly the "Unsharing Objects" section
    proc K {x y} {return $x}

    # find the distinct items in a list without changing the order
    proc ldistinct {list} {
        set uniq {}
        foreach elem $list {dict set uniq $elem ""}
        dict keys $uniq
    }
}

namespace import BookStore::basketCost
