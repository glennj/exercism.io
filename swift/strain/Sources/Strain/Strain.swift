extension Array {
    func keep(_ f: (Element) -> Bool) -> [Element] {
        var result = [Element]()
        for elem in self {
            if f(elem) {
                result.append(elem)
            }
        }
        return result
    }

    func discard(_ f: (Element) -> Bool) -> [Element] {
        return self.keep { !f($0) }
    }
}