source "dict.tcl"       ;# for `dict getdef`

oo::class create User {
    variable _name _owes _owed_by _balance
    constructor {name {owes {}} {owed_by {}} {balance 0}} {
        set _name    $name
        set _owes    $owes
        set _owed_by $owed_by
        set _balance $balance
    }
    method name {} {return $_name}

    method toDict {} {
        list \
            name    $_name \
            owes    [lsort -stride 2 $_owes] \
            owed_by [lsort -stride 2 $_owed_by] \
            balance $_balance
    }

    method borrow {lender amount} {
        set lenderName [$lender name]
        set iOweLender [dict getdef $_owes $lenderName 0]
        set lenderOwesMe [dict getdef $_owed_by $lenderName 0]
        set amountOwing [expr {$iOweLender - $lenderOwesMe + $amount}]
        dict unset _owes    $lenderName
        dict unset _owed_by $lenderName

        if {$amountOwing > 0} {
            dict set _owes $lenderName $amountOwing
        } elseif {$amountOwing < 0} {
            dict set _owed_by $lenderName [expr {-$amountOwing}]
        }

        incr _balance [expr {-$amount}]
        $lender lend [self] $amount
    }

    method lend {borrower amount} {
        set borrowerName [$borrower name]
        set iOweBorrower [dict getdef $_owes $borrowerName 0]
        set borrowerOwesMe [dict getdef $_owed_by $borrowerName 0]
        set amountOwing [expr {$borrowerOwesMe - $iOweBorrower + $amount}]
        dict unset _owes    $borrowerName
        dict unset _owed_by $borrowerName

        if {$amountOwing > 0} {
            dict set _owed_by $borrowerName $amountOwing
        } elseif {$amountOwing < 0} {
            dict set _owes $borrowerName [expr {-$amountOwing}]
        }

        incr _balance $amount
    }
}
