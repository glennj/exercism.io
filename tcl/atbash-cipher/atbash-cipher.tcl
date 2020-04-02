namespace eval atbash {
    namespace export encode decode
    namespace ensemble create

    variable map {
        a z   b y   c x   d w   e v   f u   g t   h s   i r
        j q   k p   l o   m n   n m   o l   p k   q j   r i
        s h   t g   u f   v e   w d   x c   y b   z a
    }

    proc encode {input} {
        group [decode $input]
    }

    proc decode {input} {
        encipher [alnum $input]
    }

    proc encipher {input} {
        variable map
        string map $map $input
    }

    proc alnum {input} {
        regsub -all {[^a-z0-9]} [string tolower $input] ""
    }

    proc group {input {size 5}} {
        join [regexp -all -inline ".{1,$size}" $input] { }
    }
}
