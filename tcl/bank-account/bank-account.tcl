oo::class create BankAccount {
    variable balance
    variable isOpen

    constructor {} {
        set isOpen no
    }

    method open {} {
        if {$isOpen} {
            error "This account is already open."
        }
        set balance 0
        set isOpen yes
    }

    method close {} {
        if {!$isOpen} {
            error "This account is not open."
        }
        set isOpen no
    }

    method balance {} {
        if {!$isOpen} {
            error "This account is not open."
        }
        return $balance
    }

    method deposit {amount} {
        if {!$isOpen} {
            error "This account is not open."
        }
        if {$amount < 0} {
            error "Cannot complete deposit: invalid amount."
        }
        incr balance $amount
    }

    method withdraw {amount} {
        if {!$isOpen} {
            error "This account is not open."
        }
        if {$amount < 0} {
            error "Cannot complete withdrawal: invalid amount."
        }
        if {$amount > $balance} {
            error "Cannot complete withdrawal: insufficient funds."
        }
        incr balance [expr {-1 * $amount}]
    }
}
