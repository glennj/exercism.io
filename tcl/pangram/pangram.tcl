#!/usr/bin/env tclsh

proc isPangram {text} {
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
