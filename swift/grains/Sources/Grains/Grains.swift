enum GrainsError: Error {
    case inputTooHigh(String)
    case inputTooLow(String)
}

/*
extension Int {
    func raisedTo(the exp: Int) -> Int {
        var product = 1
        if exp > 0 {
            for _ in (1...exp) {
                product *= self
            }
        }
        return product
    }
}
*/

let total = UInt64.max            // 2.raisedTo(the: 64) - 1

func square(_ n: Int) throws -> UInt64 {
    try validateInput(n: n)
    return 1 << UInt64(n - 1)     // 2.raisedTo(the: n - 1)
}

func validateInput(n: Int) throws -> Void {
    let msg = "Input[\(n)] invalid. Input should be between 1 and 64 (inclusive)"
    guard n >= 1 else {
        throw GrainsError.inputTooLow(msg)
    }
    guard n <= 64 else {
        throw GrainsError.inputTooHigh(msg)
    }
}