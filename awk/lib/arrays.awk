# Including assert means an END block exists, which means if
# we provide no files on the cmdline, awk will block on stdin.
# That precludes us from oneliners
#
#   gawk -i lib/arrays 'BEGIN {arrays::pprint(PROCINFO)}'
#
# without redirecting from devnull
##@include "assert"

@namespace "arrays"

# Unfortunately, awk doesn't have array literals, so
# there's a bit of a hoop to jump through to get a variable
# to have type "array"

function init(array, key) {
    awk::assert(CanBeArray(array),
            "error: variable of type " awk::typeof(array) " cannot be an array")
    if (awk::typeof(key) == "untyped")
        clear(array)
    else {
        # for an array-of-arrays, have to ensure the value
        # at the key is not "unassigned", else `clear` will abort
        array[key][1] = 1
        delete array[key][1]
        clear(array[key])
    }
}

function CanBeArray(a) {
    switch (awk::typeof(a)) {
        case "array":
        case "untyped":
            return 1
            break
        default:
            return 0
    }
}

function clear(array) {
    delete array
    array[1] = 1
    delete array[1]
}

function isempty(array) {
    return length(array) == 0
}

############################################################
function push(array, element) {
    array[length(array) + 1] = element
}

function unshift(array, element,    i) {
    for (i = length(array); i >= 1; i--)
        array[i + 1] = array[i]
    array[1] = element
}

function pop(array,    element) {
    element = array[length(array)]
    delete array[length(array)]
    return element
}

function shift(array,    element, len, i) {
    element = array[1]
    len = length(array)
    for (i = 2; i <= len; i++) 
        array[i - 1] = array[i]
    delete array[len]
    return element
}

function peek(array) {
    # useful for stacks
    return array[length(array)]
}

############################################################
function map(array, funcname, result,    len, i) {
    len = length(array)
    for (i = 1; i <= len; i++)
        push(result, @funcname(array[i]))
}

function filter(array, funcname, result,    len, i) {
    len = length(array)
    for (i = 1; i <= len; i++)
        if (@funcname(array[i]))
            push(result, array[i])
}

function reduce(array, funcname, initial,    acc, i) {
    acc = initial
    for (i = 1; i <= length(array); i++)
        acc = @funcname(acc, array[i])
    return acc
}

function reverse(array,   i, j, tmp) {
    i = 1
    j = length(array)
    while (i < j) {
        tmp = array[i]
        array[i] = array[j]
        array[j] = tmp
        i++; j--
    }
}

############################################################
# join
# - this differs from the included join function by putting
#   the sep argument earlier, assuming that you usually want
#   to join the whole array
function join(array, sep, start, end,    result, i) {
    awk::assert(awk::isarray(array), "can only join arrays")
    if (awk::typeof(sep)   == "untyped") sep = OFS
    if (awk::typeof(start) == "untyped") start = 1
    if (awk::typeof(end)   == "untyped") end = length(array)

    result = array[start]
    for (i = start + 1; i <= end; i++)
        result = result sep array[i]
    return result
}

############################################################
# split2assoc
# - splits a string and assigns the _values_ to the _keys_ of an associative array.
function split2assoc(str, assoc, sep,    n, a, i) {
    if (awk::typeof(sep) == "untyped") sep = FS
    n = split(str, a, sep)
    for (i = 1; i <= n; i++) assoc[a[i]]
}

############################################################
# print an array's keys and values
function pprint(array, indent,    maxw, i, val) {
    if (awk::isarray(array)) {
        maxw = -1
        for (i in array)
            if (length(i) > maxw)
                maxw = length(i)
        for (i in array) {
            switch (awk::typeof(array[i])) {
                case "array":      val = "<an array of length " length(array[i]) ">"; break
                case "unassigned": val = "<unassigned>"; break
                default:           val = array[i]
            }
            printf "%s%-*s = %s\n", indent, maxw + 2, "[" i "]", val
            pprint(array[i], indent "  ")
        }
    }
}

