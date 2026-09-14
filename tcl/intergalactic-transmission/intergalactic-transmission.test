#!/usr/bin/env tclsh
# generated: 2026-07-24T19:22:56Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "intergalactic-transmission.tcl"


# Inputs and expected data are given in hexadecimal

test intergalactic-transmission-1 "calculate transmit sequences: empty message" -body {
    transmitSequence {}
} -returnCodes ok -match orderedLists -result {}

skip intergalactic-transmission-2
test intergalactic-transmission-2 "calculate transmit sequences: 0x00 is transmitted as 0x0000" -body {
    transmitSequence {
        0x00
    }
} -returnCodes ok -match orderedLists -result {
        0x00
        0x00
    }

skip intergalactic-transmission-3
test intergalactic-transmission-3 "calculate transmit sequences: 0x02 is transmitted as 0x0300" -body {
    transmitSequence {
        0x02
    }
} -returnCodes ok -match orderedLists -result {
        0x03
        0x00
    }

skip intergalactic-transmission-4
test intergalactic-transmission-4 "calculate transmit sequences: 0x06 is transmitted as 0x0600" -body {
    transmitSequence {
        0x06
    }
} -returnCodes ok -match orderedLists -result {
        0x06
        0x00
    }

skip intergalactic-transmission-5
test intergalactic-transmission-5 "calculate transmit sequences: 0x05 is transmitted as 0x0581" -body {
    transmitSequence {
        0x05
    }
} -returnCodes ok -match orderedLists -result {
        0x05
        0x81
    }

skip intergalactic-transmission-6
test intergalactic-transmission-6 "calculate transmit sequences: 0x29 is transmitted as 0x2881" -body {
    transmitSequence {
        0x29
    }
} -returnCodes ok -match orderedLists -result {
        0x28
        0x81
    }

skip intergalactic-transmission-7
test intergalactic-transmission-7 "calculate transmit sequences: 0xc001c0de is transmitted as 0xc000711be1" -body {
    transmitSequence {
        0xc0
        0x01
        0xc0
        0xde
    }
} -returnCodes ok -match orderedLists -result {
        0xc0
        0x00
        0x71
        0x1b
        0xe1
    }

skip intergalactic-transmission-8
test intergalactic-transmission-8 "calculate transmit sequences: six byte message" -body {
    transmitSequence {
        0x47
        0x72
        0x65
        0x61
        0x74
        0x21
    }
} -returnCodes ok -match orderedLists -result {
        0x47
        0xb8
        0x99
        0xac
        0x17
        0xa0
        0x84
    }

skip intergalactic-transmission-9
test intergalactic-transmission-9 "calculate transmit sequences: seven byte message" -body {
    transmitSequence {
        0x47
        0x72
        0x65
        0x61
        0x74
        0x31
        0x21
    }
} -returnCodes ok -match orderedLists -result {
        0x47
        0xb8
        0x99
        0xac
        0x17
        0xa0
        0xc5
        0x42
    }

skip intergalactic-transmission-10
test intergalactic-transmission-10 "calculate transmit sequences: eight byte message" -body {
    transmitSequence {
        0xc0
        0x01
        0x13
        0x37
        0xc0
        0xde
        0x21
        0x21
    }
} -returnCodes ok -match orderedLists -result {
        0xc0
        0x00
        0x44
        0x66
        0x7d
        0x06
        0x78
        0x42
        0x21
        0x81
    }

skip intergalactic-transmission-11
test intergalactic-transmission-11 "calculate transmit sequences: twenty byte message" -body {
    transmitSequence {
        0x45
        0x78
        0x65
        0x72
        0x63
        0x69
        0x73
        0x6d
        0x20
        0x69
        0x73
        0x20
        0x61
        0x77
        0x65
        0x73
        0x6f
        0x6d
        0x65
        0x21
    }
} -returnCodes ok -match orderedLists -result {
        0x44
        0xbd
        0x18
        0xaf
        0x27
        0x1b
        0xa5
        0xe7
        0x6c
        0x90
        0x1b
        0x2e
        0x33
        0x03
        0x84
        0xee
        0x65
        0xb8
        0xdb
        0xed
        0xd7
        0x28
        0x84
    }

skip intergalactic-transmission-12
test intergalactic-transmission-12 "decode received messages: empty message" -body {
    decodeMessage {}
} -returnCodes ok -match orderedLists -result {}

skip intergalactic-transmission-13
test intergalactic-transmission-13 "decode received messages: zero message" -body {
    decodeMessage {
        0x00
        0x00
    }
} -returnCodes ok -match orderedLists -result {
        0x00
    }

skip intergalactic-transmission-14
test intergalactic-transmission-14 "decode received messages: 0x0300 is decoded to 0x02" -body {
    decodeMessage {
        0x03
        0x00
    }
} -returnCodes ok -match orderedLists -result {
        0x02
    }

skip intergalactic-transmission-15
test intergalactic-transmission-15 "decode received messages: 0x0581 is decoded to 0x05" -body {
    decodeMessage {
        0x05
        0x81
    }
} -returnCodes ok -match orderedLists -result {
        0x05
    }

skip intergalactic-transmission-16
test intergalactic-transmission-16 "decode received messages: 0x2881 is decoded to 0x29" -body {
    decodeMessage {
        0x28
        0x81
    }
} -returnCodes ok -match orderedLists -result {
        0x29
    }

skip intergalactic-transmission-17
test intergalactic-transmission-17 "decode received messages: first byte has wrong parity" -body {
    decodeMessage {
        0x07
        0x00
    }
} -returnCodes error -result "wrong parity"
skip intergalactic-transmission-18
test intergalactic-transmission-18 "decode received messages: second byte has wrong parity" -body {
    decodeMessage {
        0x03
        0x68
    }
} -returnCodes error -result "wrong parity"
skip intergalactic-transmission-19
test intergalactic-transmission-19 "decode received messages: 0xcf4b00 is decoded to 0xce94" -body {
    decodeMessage {
        0xcf
        0x4b
        0x00
    }
} -returnCodes ok -match orderedLists -result {
        0xce
        0x94
    }

skip intergalactic-transmission-20
test intergalactic-transmission-20 "decode received messages: 0xe2566500 is decoded to 0xe2ad90" -body {
    decodeMessage {
        0xe2
        0x56
        0x65
        0x00
    }
} -returnCodes ok -match orderedLists -result {
        0xe2
        0xad
        0x90
    }

skip intergalactic-transmission-21
test intergalactic-transmission-21 "decode received messages: six byte message" -body {
    decodeMessage {
        0x47
        0xb8
        0x99
        0xac
        0x17
        0xa0
        0x84
    }
} -returnCodes ok -match orderedLists -result {
        0x47
        0x72
        0x65
        0x61
        0x74
        0x21
    }

skip intergalactic-transmission-22
test intergalactic-transmission-22 "decode received messages: seven byte message" -body {
    decodeMessage {
        0x47
        0xb8
        0x99
        0xac
        0x17
        0xa0
        0xc5
        0x42
    }
} -returnCodes ok -match orderedLists -result {
        0x47
        0x72
        0x65
        0x61
        0x74
        0x31
        0x21
    }

skip intergalactic-transmission-23
test intergalactic-transmission-23 "decode received messages: last byte has wrong parity" -body {
    decodeMessage {
        0x47
        0xb8
        0x99
        0xac
        0x17
        0xa0
        0xc5
        0x43
    }
} -returnCodes error -result "wrong parity"
skip intergalactic-transmission-24
test intergalactic-transmission-24 "decode received messages: eight byte message" -body {
    decodeMessage {
        0xc0
        0x00
        0x44
        0x66
        0x7d
        0x06
        0x78
        0x42
        0x21
        0x81
    }
} -returnCodes ok -match orderedLists -result {
        0xc0
        0x01
        0x13
        0x37
        0xc0
        0xde
        0x21
        0x21
    }

skip intergalactic-transmission-25
test intergalactic-transmission-25 "decode received messages: twenty byte message" -body {
    decodeMessage {
        0x44
        0xbd
        0x18
        0xaf
        0x27
        0x1b
        0xa5
        0xe7
        0x6c
        0x90
        0x1b
        0x2e
        0x33
        0x03
        0x84
        0xee
        0x65
        0xb8
        0xdb
        0xed
        0xd7
        0x28
        0x84
    }
} -returnCodes ok -match orderedLists -result {
        0x45
        0x78
        0x65
        0x72
        0x63
        0x69
        0x73
        0x6d
        0x20
        0x69
        0x73
        0x20
        0x61
        0x77
        0x65
        0x73
        0x6f
        0x6d
        0x65
        0x21
    }

skip intergalactic-transmission-26
test intergalactic-transmission-26 "decode received messages: wrong parity on 16th byte" -body {
    decodeMessage {
        0x44
        0xbd
        0x18
        0xaf
        0x27
        0x1b
        0xa5
        0xe7
        0x6c
        0x90
        0x1b
        0x2e
        0x33
        0x03
        0x84
        0xef
        0x65
        0xb8
        0xdb
        0xed
        0xd7
        0x28
        0x84
    }
} -returnCodes error -result "wrong parity"

cleanupTests
