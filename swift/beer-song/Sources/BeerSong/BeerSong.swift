extension String {
    func capitalize() -> String {
        if self.isEmpty { return self }
        return self.first!.uppercased()
             + String(self.suffix(self.count - 1))
    }
}

struct BeerSong {
    private let num: Int
    init(numberOfBeerBottles: Int) {
        self.num = numberOfBeerBottles
    }

    func generateVersesOfBeerSong() -> String {
        var verses: [String] = []
        for i in stride(from: self.num, to: -1, by: -1) {
            var b = self.bottle(i)
            let first = "\(b.capitalize()) on the wall, \(b)."
            b = self.bottle(i == 0 ? 99 : i - 1)
            let second = "\(self.task(i)), \(b) on the wall."
            verses.append("\(first)\n\(second)")
        }
        return verses.joined(separator: "\n\n")
    }

    private func bottle(_ i: Int) -> String {
        let n = i == 0 ? "no more" : String(i)
        let s = i == 1 ? "" : "s"
        return "\(n) bottle\(s) of beer"
    }

    private func task(_ i: Int) -> String {
        let one = i == 1 ? "it" : "one"
        return i == 0
            ? "Go to the store and buy some more" 
            : "Take \(one) down and pass it around"
    }
}
