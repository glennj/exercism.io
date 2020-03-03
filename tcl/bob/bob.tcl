proc heyBob {phrase} {
    set p [string trim $phrase]

    if {$p eq ""} { return "Fine. Be that way!" }

    set q [string match {*\?} $p]

    set s [expr {
         [regexp {[[:upper:]]} $p] && 
        ![regexp {[[:lower:]]} $p]
    }]

    if {$q && $s} { return "Calm down, I know what I'm doing!" }
    if {$q}       { return "Sure." }
    if {$s}       { return "Whoa, chill out!" }

    return "Whatever."
}
