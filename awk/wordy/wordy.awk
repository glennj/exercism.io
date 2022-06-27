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

FNR == 1 { assert($0 == "What", "unknown operation"); next }
FNR == 2 { assert($0 == "is",   "unknown operation"); next }

state == "NUM" {
    assert(isnumber($0), "syntax error")
    result = @operation(result, $0)
    state = "OP"
    next
}

state == "OP" {
    assert(!isnumber($0), "syntax error")
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
    operation = $0
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

function assert(cond, msg) {
    if (!cond) die(msg)
}
