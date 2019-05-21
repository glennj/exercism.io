extension Int {
    enum Parity: Int {
        case even, odd
        init(number: Int) {self.init(rawValue: number % 2)!}
    }

    var parity: Parity {
        return Parity(number: self)
    }
}

enum CollatzError: Error {
    case invalidInput
}

func steps(_ input: Int) throws -> Int {
    if input <= 0 {throw CollatzError.invalidInput}
    var n = input
    var steps = 0
    while n != 1 {
        switch n.parity {
            case .even: n /= 2
            case .odd:  n = 3 * n + 1
        }
        steps += 1
    }
    return steps
}