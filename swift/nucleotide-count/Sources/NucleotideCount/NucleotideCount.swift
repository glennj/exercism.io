struct DNA {
    private var counter: [String: Int]

    init?(strand: String) {
        self.counter = ["T": 0, "A": 0, "C": 0, "G": 0]

        for char in strand {
            let s = String(char)
            guard self.counter.keys.contains(s) else {
                return nil
            }
            self.counter[s]! += 1
        }
    }

    func count(_ char: String) -> Int? {
        return self.counter[char]
    }

    func counts() -> [String: Int] {
        // dictionaries are value types, so returning it
        // returns a copy not a reference.
        return self.counter
    }
}