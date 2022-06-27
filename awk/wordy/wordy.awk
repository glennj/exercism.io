#!/usr/bin/env gawk -f

BEGIN {
    RS = "[[:space:]]+"
    exit_status = 0
    state = "NUM"
    result = 0
    operation = "plus"
}

skip-- > 0 {next}

{ sub(/\?$/, "") }

FNR == 1 && $0 != "What" { die("unknown operation") }
FNR == 2 && $0 != "is"   { die("unknown operation") }
FNR <= 2 {next}

state == "NUM" {
    if (!isnumber($0)) die("syntax error")
    result = @operation(result, $0)
    state = "OP"
    next
}

state == "OP" {
    if (isnumber($0)) die("syntax error")
    operation = $0
    switch ($0) {
        case "plus":
        case "minus":
            break
        case "multiplied":
        case "divided":
            skip = 1     # TODO, check next word is "by"
            break
        default:
            die("unknown operation")
    }
    state = "NUM"
    next
}

END {
    if (exit_status == 0) {
        if (state == "NUM")
            error("syntax error")
        else
            print result
    }
    exit exit_status
}

############################################################
function isnumber(str) {
    return (str ~ /^[+-]?[[:digit:]]+$/)
}

function plus(a, b)       {return a + b}
function minus(a, b)      {return a - b}
function multiplied(a, b) {return a * b}
function divided(a, b)    {return a / b}   # TODO, division by zero

function error(msg) {
    print msg > "/dev/stderr"
    exit_status = 1
}

function die(msg) {
    error(msg)
    exit
}
