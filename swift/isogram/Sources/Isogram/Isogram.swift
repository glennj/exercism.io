func isIsogram(_ input: String) -> Bool {
    var str = input.lowercased()
    str.removeAll { !$0.isLetter }
    return str.count == Set(str).count
}