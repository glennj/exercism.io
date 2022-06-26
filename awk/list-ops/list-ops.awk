@namespace "listops"

# Append to a list all the elements of another list.
# Or append to a list a single new element
function append(list, item_or_list,    n, i) {
    n = length(list)
    if (awk::isarray(item_or_list)) 
        for (i = 1; i <= length(item_or_list); i++)
            list[++n] = item_or_list[i]
    else
        list[++n] = item_or_list
}

# Concatenate is flattening a list of lists one level
function concat(list, result) {
    for (i = 1; i <= length(list); i++)
        append(result, list[i])
}

# Only the list elements that pass the given function.
function filter(list, funcname, result,    i, n) {
    for (i = 1; i <= length(list); i++)
        if (@funcname(list[i]))
            result[++n] = list[i]
}

# Transform the list elements, using the given function, into a new list.
function map(list, funcname, result,    i, n) {
    for (i = 1; i <= length(list); i++)
        result[++n] = @funcname(list[i])
}

# Left-fold the list using the function and the initial value.
function foldl(list, funcname, initial,    acc, i) {
    acc = initial
    for (i = 1; i <= length(list); i++)
        acc = @funcname(acc, list[i])
    return acc
}

# Right-fold the list using the function and the initial value.
function foldr (list, funcname, initial,    acc, i) {
    acc = initial
    for (i = length(list); i >= 1; i--)
        acc = @funcname(list[i], acc)
    return acc
}

# the list reversed
function reverse (list, result,    i, n) {
    for (i = length(list); i >= 1; i--)
        result[++n] = list[i]
}
