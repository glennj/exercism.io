namespace eval Satellite {
    namespace export treeFromTraversals

    proc treeFromTraversals {preorder inorder} {

        validate $preorder $inorder

        if {$preorder eq ""} then {return {}}

        # the root of the tree is the first element of the preorder
        set root [lindex $preorder 0]

        # find the root in the inorder list so we can determine
        # the left and right branches
        set idx [lsearch -exact $inorder $root]
        assert {$idx > -1} "Error: root not found in inorder list"

        set lIn [lrange $inorder 0 $idx-1]
        set rIn [lrange $inorder $idx+1 end]

        set lPre [lrange $preorder 1 $idx]
        set rPre [lrange $preorder $idx+1 end]

        # proc name, for recursive call
        set tft [lindex [info level 0] 0]

        set tree {}
        dict set tree v $root
        dict set tree l [expr {$lIn eq "" ? {} : [$tft $lPre $lIn]}]
        dict set tree r [expr {$rIn eq "" ? {} : [$tft $rPre $rIn]}]
        return $tree
    }

    proc validate {preorder inorder} {
        assert {[llength $preorder] == [llength $inorder]} \
            "traversals must have the same length"

        set preCount {}
        foreach elem $preorder {
            assert {![dict exists $preCount $elem]} \
                "traversals must contain unique elements"
            dict set preCount $elem 1
        }

        set inCount {}
        foreach elem $inorder {
            assert {![dict exists $inCount $elem]} \
                "traversals must contain unique elements"
            assert {[dict exists $preCount $elem]} \
                "traversals must contain the same elements"
            dict set inCount $elem 1
        }
    }

    proc assert {condition errMsg} {
        if {![uplevel 1 [list expr $condition]]} {
            error $errMsg
        }
    }
}

namespace import Satellite::treeFromTraversals
