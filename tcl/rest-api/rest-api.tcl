source "dict.tcl"       ;# for `dict getdef`
source "iou-user.tcl"   ;# User class

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

#############################################################
oo::class create RestAPI {
    variable db
    variable routes

    constructor {data} {
        set db {}
        foreach userinfo [dict getdef $data users {}] {
            dict with userinfo {
                dict set db $name [User new $name $owes $owed_by $balance]
            }
        }
        # reminder, only methods starting with [a-z] are exported by default
        set routes {
            GET {
                /users  GetUsers
            }
            POST {
                /add    PostAdd
                /iou    PostIou
            }
        }
    }

    # this will handle any HTTP method, get/post/head/...
    method unknown {httpMethod args} {
        my Dispatcher [string toupper $httpMethod] {*}$args
    }

    method Dispatcher {httpMethod uri {payload {}}} {
        assert {[dict exists $routes $httpMethod]}      "501 Invalid HTTP method"
        assert {[dict exists $routes $httpMethod $uri]} "404 URI not found"
        tailcall my [dict get $routes $httpMethod $uri] $payload
    }

    method GetUsers {payload} {
        if {![dict exists $payload users]} {
            set userlist [dict values $db]
        } else {
            set userlist {}
            foreach username [dict get $payload users] {
                if {[dict exists $db $username]} {
                    lappend userlist [dict get $db $username]
                } else {
                    lappend userlist [User new $username]
                }
            }
        }
        tailcall my ReturnUsers $userlist
    }

    method ReturnUsers {userlist} {
        dict create "users" [lsort -index 1 [lmap user $userlist {$user toDict}]]
    }

    method PostAdd {payload} {
        assert {[dict exists $payload user]}  "400 Missing user key"
        set username [dict get $payload user]
        assert {![dict exists $db $username]} "400 User already exists"

        set user [User new $username]
        dict set db $username $user
        $user toDict
    }

    method PostIou {payload} {
        assert {[dict exists $payload lender]}   "400 Missing lender key"
        assert {[dict exists $payload borrower]} "400 Missing borrower key"
        assert {[dict exists $payload amount]}   "400 Missing amount key"

        set lenderName   [dict get $payload lender]
        set borrowerName [dict get $payload borrower]
        set amount       [dict get $payload amount]

        assert {[dict exists $db $lenderName]}   "400 Lender does not exist"
        assert {[dict exists $db $borrowerName]} "400 Borrower does not exist"

        set lender   [dict get $db $lenderName]
        set borrower [dict get $db $borrowerName]

        $borrower borrow $lender $amount

        tailcall my ReturnUsers [list $lender $borrower]
    }
}
