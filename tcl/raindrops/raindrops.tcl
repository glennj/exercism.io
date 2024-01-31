proc raindrops {number} {
    set Pling 3
    set Plang 5
    set Plong 7

    set drops ""
    foreach varname [info vars P*] {
        if {$number % [set $varname] == 0} {
            append drops $varname
        }
    }
    expr {$drops eq "" ? $number : $drops}
}
