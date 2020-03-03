#!tclsh
#
source armstrong-numbers.tcl

foreach proc {isArmstrongNumber_string isArmstrongNumber_math} {
    puts [list $proc [time {$proc 9926315} 100000]]
}
