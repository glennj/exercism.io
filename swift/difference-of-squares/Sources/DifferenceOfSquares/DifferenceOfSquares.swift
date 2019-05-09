class Squares {
    let limit: Int

    init(_ limit: Int) {
        self.limit = limit
    }

    lazy var squareOfSum: Int = {
        let sum = (1...self.limit).reduce(0, +)
        return sum * sum
    }()

    lazy var sumOfSquares: Int = {
        let sum = (1...self.limit)
                    .map {$0 * $0}
                    .reduce(0, +)
        return sum
    }()

    lazy var differenceOfSquares: Int = {
        return self.squareOfSum - self.sumOfSquares
    }()
}
