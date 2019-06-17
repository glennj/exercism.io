enum SublistClassification {
    case equal, sublist, superlist, unequal
}

// I want to make this function generic:
//     func classifier<T: Equatable>(listOne a: [T], listTwo b: [T]) -> SublistClassification {}
// but:
//     Protocol type 'Any' cannot conform to 'Equatable' because only concrete types can conform to protocols

func classifier(listOne a: [Int], listTwo b: [Int]) -> SublistClassification {
    if a.count == b.count {
        return isSublist(a, b) ? .equal : .unequal
    } else if a.count < b.count {
        return isSublist(a, b) ? .sublist : .unequal
    } else {
        return isSublist(b, a) ? .superlist : .unequal
    }
}

// Returns true if `a` is a sublist of `b`
// The empty list `a` is always a sublist of any list

private func isSublist<T: Equatable>(_ a: [T], _ b: [T]) -> Bool {
    if a.isEmpty { return true }

    for (bIdx, bValue) in b.enumerated() {
        if bValue == a.first! {
            var found = true
            for (aIdx, aValue) in a.enumerated() {
                if (bIdx + aIdx >= b.count) || (aValue != b[bIdx + aIdx]) {
                    found = false
                    break
                }
            }
            if found { return true }
        }
    }

    return false
}
