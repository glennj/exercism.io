proc matrixFrom {inputString} {
    return [lmap line [split $inputString \n] {split $line}]
}

proc row {matrix n} {
    return [lindex $matrix $n-1]
}

proc column {matrix n} {
    return [lmap row $matrix {lindex $row $n-1}]
}
