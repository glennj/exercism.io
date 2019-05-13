typealias Position = [Int]

struct Queens {
    enum InitError: Error {
        case incorrectNumberOfCoordinates
        case invalidCoordinates
        case sameSpace
    }

    let white: Position
    let black: Position

    init(white: Position = [0,3], black: Position = [7,3]) throws {
        self.white = white
        self.black = black
        try self.validatePosition(white)
        try self.validatePosition(black)
        guard white != black else {
            throw InitError.sameSpace
        }
    }

    private func validatePosition(_ pos: Position) throws {
        guard pos.count == 2 else {
            throw InitError.incorrectNumberOfCoordinates
        }
        guard pos.allSatisfy({0 <= $0 && $0 <= 7}) else {
            throw InitError.invalidCoordinates
        }
    }

    var canAttack: Bool {
        get {
            let dx = abs(self.white[0] - self.black[0])
            let dy = abs(self.white[1] - self.black[1])
            return dx == 0 || dy == 0 || dx == dy
        }
    }

    var description: String {
        get {
            var board = Array(
                repeating: Array(repeating: "_", count: 8),
                count: 8
            )
            board[self.white[0]][self.white[1]] = "W"
            board[self.black[0]][self.black[1]] = "B"
            return board.map { $0.joined(separator: " ") }
                        .joined(separator: "\n")
        }
    }
}