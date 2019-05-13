struct Scrabble {
    static let Tiles: [Character: Int] = [
        "A": 1, "E": 1, "I": 1, "O": 1, "U": 1,
        "L": 1, "N": 1, "R": 1, "S": 1, "T": 1,
        "D": 2, "G": 2,
        "B": 3, "C": 3, "M": 3, "P": 3,
        "F": 4, "H": 4, "V": 4, "W": 4, "Y": 4,
        "K": 5,
        "J": 8, "X": 8,
        "Q": 10, "Z": 10,
    ]

    private(set) var score = 0

    init(_ input: String?) {
        for char in (input ?? "").uppercased() {
            if let value = Scrabble.Tiles[char] {
                score += value
            }
        }
    }

    static func score(_ input: String) -> Int {
        return Scrabble(input).score
    }
}


/* community:

extend Character to add a "value" property for cap letters
https://exercism.io/tracks/swift/exercises/scrabble-score/solutions/6e22ae7948fe4b1199cdab53d0ecf201

*/