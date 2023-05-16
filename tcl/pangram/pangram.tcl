#!/usr/bin/env tclsh

proc isPangram {text} {
    #return [isPangram_bitfield $text]
    return [isPangram_stringsearch $text]
}

set A [scan A %c]                                   ;# ASCII 65
set expected [expr {0b11111111111111111111111111}]  ;# 26 ones

proc isPangram_bitfield {text} {
    global A expected
    set result 0
    foreach char [split [string toupper $text] ""] {
        if {[string match {[A-Z]} $char]} {
            set result [expr {$result | (1 << ([scan $char %c] - $A))}]
        }
    }
    return [expr {$result == $expected}]
}

proc isPangram_stringsearch {text} {
    # using http://pi.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
    # look at the letters most likely to be absent first.
    set text [string tolower $text]
    foreach letter {z j q x k v b p g w y f m c u l d h r s n i o a t e} {
        if {[string first $letter $text] == -1} {
            return 0
        }
    }
    return 1
}


if 0 {
    Using a bitfield
    ++++ pangram-1 took 225 μs
    ++++ pangram-2 took 52 μs
    ++++ pangram-3 took 56 μs
    ++++ pangram-4 took 66 μs
    ++++ pangram-5 took 52 μs
    ++++ pangram-6 took 55 μs
    ++++ pangram-7 took 54 μs
    ++++ pangram-8 took 49 μs
    ++++ pangram-9 took 51 μs
    ++++ pangram-10 took 44 μs


    Using string first
    ++++ pangram-1 took 201 μs
    ++++ pangram-2 took 32 μs
    ++++ pangram-3 took 30 μs
    ++++ pangram-4 took 50 μs
    ++++ pangram-5 took 27 μs
    ++++ pangram-6 took 31 μs
    ++++ pangram-7 took 27 μs
    ++++ pangram-8 took 26 μs
    ++++ pangram-9 took 28 μs
    ++++ pangram-10 took 23 μs
}
