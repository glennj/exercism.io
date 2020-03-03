#!tclsh
#
source difference-of-squares.tcl

foreach ns {difference::math difference::iterative} {
    puts [list $ns [time {${ns}::differenceOfSquares 500} 1000]]
}
