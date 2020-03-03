proc score {x y} {
    set distance [expr {hypot($x, $y)}]
    if {$distance <=  1} {return 10}
    if {$distance <=  5} {return  5}
    if {$distance <= 10} {return  1}
    return 0
}
