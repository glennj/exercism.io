oo::class create BankAccount {
    variable balance
    variable isOpen

    constructor {} {
        set isOpen no
        set balance 0
    }

    method open {} {
        assert {!$isOpen} "account already open"
        set isOpen yes
    }

    method close {} {
        assert {$isOpen} "account not open"
        my withdraw $balance
        set isOpen no
    }

    method balance {} {
        assert {$isOpen} "account not open"
        return $balance
    }
 
    method deposit {amount} {
        assert {$isOpen} "account not open"
        assert {$amount >= 0} "amount must be greater than 0"
        incr balance $amount
    }

    method withdraw {amount} {
        assert {$isOpen} "account not open"
        assert {$amount >= 0} "amount must be greater than 0"
        assert {$amount <= $balance} "amount must be less than balance"
        incr balance [expr {-1 * $amount}]
    }
}


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}
