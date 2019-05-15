enum SublistClassification {
    case equal, sublist, superlist, unequal
}

func classifier<T>(listOne a: [T], listTwo b: [T]) -> SublistClassification {
    if a.count == b.count {
        return isSublist(a, b) ? .equal : .unequal
    } else if a.count < b.count {
        return isSublist(a, b) ? .sublist : .unequal
    } else {
        return isSublist(b, a) ? .superlist : .unequal
    }
}

private func isSublist<T>(_ a: [T], _ b: [T]) -> Bool {
    var bList = b
    if a.isEmpty { return true }

    var idx = b.firstIndex(of: a.first!)

    while idx != nil {
        var found = true
        for (i, val) in a.enumerated() {
            if idx + i >= bList.count {
                return false
            }
            if val != b[idx + i] {
                found = false
                break
            }
        }
        if found { return true }
        

    }

    return false
}