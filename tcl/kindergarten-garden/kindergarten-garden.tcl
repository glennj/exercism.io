proc plants {diagram student} {
    set idx [lsearch -exact {
        Alice  Bob      Charlie  David   Eve      Fred 
        Ginny  Harriet  Ileana   Joseph  Kincaid  Larry
    } $student]
    if {$idx == -1} then return

    lassign [lmap row [split $diagram \n] {split $row ""}] row1 row2
    set plots [lmap {a b} $row1 {c d} $row2 {list $a $b $c $d}]

    lmap plant [lindex $plots $idx] {
        dict get {R radishes C clover G grass V violets} $plant
    }
}
