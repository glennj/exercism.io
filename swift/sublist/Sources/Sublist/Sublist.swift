enum SublistClassification {
    case equal, sublist, superlist, unequal
}

func classifier(listOne a: [Int], listTwo b: [Int]) -> SublistClassification {
    if a.count == b.count {
        return isSublist(a, b) ? .equal : .unequal
    } else if a.count < b.count {
        return isSublist(a, b) ? .sublist : .unequal
    } else {
        return isSublist(b, a) ? .superlist : .unequal
    }
}

private func isSublist<T: Equatable>(_ a: [T], _ b: [T]) -> Bool {
    let bList = b
    if a.isEmpty { return true }

    // let idx = b.firstIndex(of: a.first!)
    let idx = b.firstIndex(where: {$0 == a.first!})

    while idx != nil {
        var found = true
        for (i, val) in a.enumerated() {
            if idx! + i >= bList.count {
                return false
            }
            if val != b[idx! + i] {
                found = false
                break
            }
        }
        if found { return true }
    }

    return false
}
