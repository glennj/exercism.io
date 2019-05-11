func compute(_ a: String, against b: String) -> Int? {
    if a.count != b.count {return nil}

    /*
    var diff = 0
    var aIter = a.makeIterator()
    var bIter = b.makeIterator()

    while let aChar = aIter.next(), let bChar = bIter.next() {
        if aChar != bChar {diff += 1}
    }
    return diff
    */

    return zip(a, b).reduce(0) {
        (count, pair) in
        count + (pair.0 == pair.1 ? 0 : 1)
    }
}