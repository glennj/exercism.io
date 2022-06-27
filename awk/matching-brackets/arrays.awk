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

function push(array, element) {
    array[length(array) + 1] = element
}

function pop(array,    element) {
    element = array[length(array)]
    delete array[length(array)]
    return element
}

function isempty(array) {
    return length(array) == 0
}
