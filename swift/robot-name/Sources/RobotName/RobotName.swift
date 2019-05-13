class Robot {
    private static var namesDb = Set<String>()
    private static let letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    private static let numbers = "0123456789"
    private(set) var name = ""

    init() {
        self.resetName()
    }

    func resetName() -> Void {
        var name: String
        repeat {
            name = String(Robot.letters.shuffled().prefix(2))
                 + String(Robot.numbers.shuffled().prefix(3))
        } while Robot.namesDb.contains(name)
        Robot.namesDb.insert(name)
        self.name = name
    }
}