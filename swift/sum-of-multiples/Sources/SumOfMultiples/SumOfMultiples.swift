func toLimit(_ limit: Int, inMultiples: [Int]) -> Int {
    /*
    var set: Set<Int> = []
    for m in inMultiples.drop(while: {$0 <= 0}) {
        //var mult = m
        //while mult < limit {
        //    set.insert(mult)
        //    mult += m
        //}
        set = set.union( stride(from: m, to: limit, by: m) )
    }
    return set.reduce(0, +)
    */
    return (0..<limit)
        .filter {n in
            inMultiples.contains {m in n.isMultiple(of: m)}
        }
        .reduce(0, +)
}
