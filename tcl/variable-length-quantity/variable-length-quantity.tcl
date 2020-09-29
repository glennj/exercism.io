namespace eval VariableLengthQuantity {
    namespace export encode decode

    variable BITS 7
    variable MASK 0b01111111
    variable MSB  0b10000000

    proc encode {numbers} {
        variable BITS
        variable MASK
        variable MSB

        set bytes {}
        foreach n $numbers {
            set theseBytes [list]

            # least significant byte
            set lsb [expr {$n & $MASK}]
            set n [expr {$n >> $BITS}]
            lappend theseBytes [asHex $lsb]

            while {$n > 0} {
                set b [expr {($n & $MASK) | $MSB}]
                set n [expr {$n >> $BITS}]
                lappend theseBytes [asHex $b]
            }
            lappend bytes {*}[lreverse $theseBytes]
        }
        return $bytes
    }

    proc decode {bytes} {
        assert {[isLSB [lindex $bytes end]]} "incomplete sequence"

        variable BITS
        variable MASK
        set num 0
        lmap byte $bytes {
            set num [expr {($num << $BITS) | ($byte & $MASK)}]
            # keep accumulating until we find the least significant byte
            if {![isLSB $byte]} then continue
            # found the LSB
            set quantity [asHex $num {%X}]
            set num 0
            string cat $quantity
        }
    }

    proc isLSB {byte} {
        variable MSB
        expr {($byte & $MSB) == 0}
    }

    proc asHex {value {fmt {%02X}}} {
        format "0x$fmt" $value
    }
}

proc assert {condition errMsg} {
    if {![uplevel 1 [list expr $condition]]} {
        error $errMsg
    }
}

namespace import VariableLengthQuantity::*
