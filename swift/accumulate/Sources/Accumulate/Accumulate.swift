extension Array {
    func accumulate<T>(_ f: (Element) -> T) -> [T] {
        var result = [T]()
        for elem in self {
            result.append( f(elem) )
        }
        return result
    }
}
