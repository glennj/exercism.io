@namespace "arrays"

# Unfortunately, awk doesn't have array literals, so
# there's a bit of a hoop to jump through to get a variable
# to have type "array"

function init(array,    t) {
    t = awk::typeof(array)
    if (!(t == "array" || t == "untyped")) 
        print "error: variable of type " t " cannot be an array"
    clear(array)
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

function reverse(array,   len, i, j, tmp) {
    len = length(array)
    for (i = 1; i <= int(len / 2); i++) {
        j = len - i + 1
        tmp = array[i]
        array[i] = array[j]
        array[j] = tmp
    }
}

############################################################
# print an array's keys and values
function pprint(array, indent,    maxw, i, val) {
    if (awk::typeof(array) != "array" )
        return
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

