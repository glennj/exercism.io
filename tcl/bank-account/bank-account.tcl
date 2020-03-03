oo::class create BankAccount {
    variable balance
    variable isOpen

    constructor {} {
        set isOpen no
    }

    method open {} {
        assert {!$isOpen} "This account is already open."
        set balance 0
        set isOpen yes
    }

    method close {} {
        assert {$isOpen} "This account is not open."
        set isOpen no
    }

    method balance {} {
        assert {$isOpen} "This account is not open."
        return $balance
    }

    method deposit {amount} {
        assert {$isOpen} "This account is not open."
        assert {$amount >= 0} "Cannot complete deposit: invalid amount."
        incr balance $amount
    }

    method withdraw {amount} {
        assert {$isOpen} "This account is not open."
        assert {$amount >= 0} "Cannot complete withdrawal: invalid amount."
        assert {$balance >= $amount} "Cannot complete withdrawal: insufficient funds."
        incr balance [expr {-1 * $amount}]
    }
}


proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}
