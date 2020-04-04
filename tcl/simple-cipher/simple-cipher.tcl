oo::class create SimpleCipher {
    variable key

    constructor {args} {
        set key [lindex $args 0]    ;# may be empty
        if {![string is lower $key]} {
            # TODO not exactly right, `string is lower` allows unicode
            error "key must only contain lower case letters"
        }
        if {$key eq ""} {
            set alphabet "abcdefghijklmnopqrstuvwxyz"
            for {set i 0} {$i < 100} {incr i} {
                append key [string index $alphabet [expr {int(rand() * 26)}]]
            }
        }
    }

    method key {} {return $key}

    forward encode  my code +1
    forward decode  my code -1

    method code {direction phrase} {
        if {![string is lower $phrase]} {
            # TODO not exactly right, `string is lower` allows unicode
            error "phrase must only contain lower case letters"
        }
        set pLen [string length $phrase]
        my keyLength $pLen

        set coded ""
        foreach p [split $phrase ""] \
                k [split [string range $key 0 $pLen-1] ""] \
        {
           append coded [my translateChar $p $k $direction]
        }
        return $coded
    }
    unexport code

    # ensure the key is at least as long as the phrase
    method keyLength {pLen} {
        set kLen [string length $key]
        while {$kLen < $pLen} {
            append key $key
            set kLen [string length $key]
        }
    }
    unexport keyLength

    method translateChar {pChar kChar direction} {
        # convert to ASCII values
        set a [scan "a" %c]
        set p [scan $pChar %c]
        set k [scan $kChar %c]

        set e [expr {(($p - $a) + $direction * ($k - $a)) % 26 + $a}]
        return [format %c $e]
    }
    unexport translateChar
}
