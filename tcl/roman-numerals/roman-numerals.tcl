proc toroman {decimal} {

    set romanize {{d r} {
        upvar 1  decimal decimal  roman roman
        while {$decimal >= $d} {
            incr decimal -$d
            append roman $r
        }
    }}

    set roman ""
    apply $romanize 1000  M
    apply $romanize  900 CM
    apply $romanize  500  D
    apply $romanize  400 CD
    apply $romanize  100  C
    apply $romanize   90 XC
    apply $romanize   50  L
    apply $romanize   40 XL
    apply $romanize   10  X
    apply $romanize    9 IX
    apply $romanize    5  V
    apply $romanize    4 IV
    apply $romanize    1  I
    return $roman
}
