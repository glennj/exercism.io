proc transmitSequence {message} {
    set messageBits [fold {} {bits byte} $message {
        concat $bits [split [format %08b $byte] ""]
    }]

    set encoded [list]
    while {[llength $messageBits] >= 7} {
        set chunk [lrange $messageBits 0 6]
        lappend encoded [toByte [concat $chunk [parity $chunk]]]
        set messageBits [lrange $messageBits 7 end]
    }

    if {[llength $messageBits] > 0} {
        set chunk $messageBits
        while {[llength $chunk] < 7} {
            lappend chunk 0
        }
        lappend encoded [toByte [concat $chunk [parity $chunk]]]
    }

    return $encoded
}

proc decodeMessage {transmission} {
    set decodedBits [list]
    foreach byte $transmission {
        set bits [split [format %08b $byte] ""]
        set chunk [lrange $bits 0 6]

        if {[parity $chunk] != [lindex $bits 7]} {
            error "wrong parity"
        }

        lappend decodedBits {*}$chunk
    }

    set decoded [list]
    while {[llength $decodedBits] >= 8} {
        lappend decoded [toByte [lrange $decodedBits 0 7]]
        set decodedBits [lrange $decodedBits 8 end]
    }

    return $decoded
}

############################################################
proc parity {bits} {
    fold 0 {sum bit} $bits {expr {$sum + $bit}}
    expr {$sum & 1}
}

proc toByte {bits} {
    format "0x%x" "0b[join $bits ""]"
}

proc fold {init varnames list script} {
    lassign $varnames accname elemname
    upvar 1 $accname accum
    upvar 1 $elemname elem

    set accum $init
    foreach elem $list {
        set accum [uplevel 1 $script]
    }
    return $accum
}
