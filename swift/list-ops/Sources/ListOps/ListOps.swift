/* encapsulate all the builtin list operations */
func push<T>(_ list: inout [T], _ elem: T) -> Void {
    list.append(elem)
}
// TODO: pop<T>(_ list: inout T) -> T

func unshift<T>(_ list: inout [T], _ elem: T) -> Void {
    list.insert(elem, at: 0)
}
// TODO: shift<T>(_ list: inout T) -> T

func forEach<T>(_ list: [T], _ f: (T) -> Void) -> Void {
    for elem in list { f(elem) }
}

/* and now, the functions we're asked to implement */
func length<T>(_ list: [T]) -> Int {
    // return list.count
    var count = 0
    forEach(list) { _ in count += 1 }
    return count
}

func append<T>(_ list: [T], _ other: [T]) -> [T] {
    var result = list
    forEach(other) { push(&result, $0) }
    return result
}

func concat<T>(_ lists: [T]...) -> [T] {
    var result = Array<T>()
    forEach(lists) { result = append(result, $0) }
    return result
}

func filter<T>(_ list: [T], _ predicate: (T) -> Bool) -> [T] {
    var result = Array<T>()
    forEach(list) { if predicate($0) { push(&result, $0) } }
    return result
}

func map<T>(_ list: [T], _ f: (T) -> T) -> [T] {
    var result = Array<T>()
    forEach(list) { push(&result, f($0)) }
    return result
}

func foldLeft<T>(_ list: [T], accumulated: T, combine: (T,T) -> T) -> T {
    var result = accumulated
    forEach(list) { result = combine(result, $0) }
    return result
}

func reverse<T>(_ list: [T]) -> [T] {
    var result = Array<T>()
    forEach(list) { unshift(&result, $0) }
    return result
}

func foldRight<T>(_ list: [T], accumulated: T, combine: (T,T) -> T) -> T {
    var result = accumulated
    forEach(reverse(list)) { result = combine($0, result) }
    return result
}
