#!/usr/bin/env tclsh
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# uncomment for more verbose test results
#configure -verbose {body error usec}

############################################################
source "satellite.tcl"

# a non-empty result value is a dictionary with keys:
#   "v"     the root value
#   "l"     the left tree
#   "r"     the right tree

test satellite-1.1 "Empty tree" -body {
    treeFromTraversals {} {}
} -returnCodes ok -match dictionary -result {}

skip satellite-1.2
test satellite-1.2 "Tree with one item" -body {
    treeFromTraversals {a} {a}
} -returnCodes ok -match dictionary -result {v a l {} r {}}

skip satellite-1.3
test satellite-1.3 "Tree with many items" -body {
    treeFromTraversals {a i x f r} {i a f x r}
} -returnCodes ok -match dictionary -result {
    v a
    l {v i l {} r {}}
    r {v x
        l {v f l {} r {}}
        r {v r l {} r {}}
    }
}


skip satellite-2.1
test satellite-2.1 "Reject traversals of different length" -body {
    treeFromTraversals {a b} {b a r}
} -returnCodes error -result "traversals must have the same length"

skip satellite-2.2
test satellite-2.2 "Reject inconsistemt traversals of same length" -body {
    treeFromTraversals {x y z} {a b c}
} -returnCodes error -result "traversals must contain the same elements"

skip satellite-2.3
test satellite-2.3 "Reject traversals with repeated elements" -body {
    treeFromTraversals {a b a} {b a a}
} -returnCodes error -result "traversals must contain unique elements"


skip sattelite-3.1
test sattelite-3.1 "A degenerate binary tree" -body {
    treeFromTraversals {a b c d} {d c b a}
} -returnCodes ok -match dictionary -result {
    v a
    l {
        v b
        l {
            v c
            l { v d l {} r {} }
            r {}
        }
        r {}
    }
    r {}
}

skip sattelite-3.2
test sattelite-3.2 "Another degenerate binary tree" -body {
    treeFromTraversals {a b c d} {a b c d}
} -returnCodes ok -match dictionary -result {
    v a
    l {}
    r {
        v b
        l {}
        r {
            v c
            l {}
            r { v d l {} r {} }
        }
    }
}

skip sattelite-3.3
test sattelite-3.3 "Tree with many more items" -body {
    set preorder {a b d g h c e f i}
    set inorder  {g d h b a e c i f}
    treeFromTraversals $preorder $inorder
} -returnCodes ok -match dictionary -result {
    v a
    l {
        v b
        l {
            v d
            l { v g l {} r {} }
            r { v h l {} r {} }
        }
        r {}
    }
    r {
        v c
        l { v e l {} r {} }
        r {
            v f
            l { v i l {} r {} }
            r {}
        }
    }
}

cleanupTests
