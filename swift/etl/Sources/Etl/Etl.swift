typealias OldType = [Int: [String]]
typealias NewType = [String: Int]

struct ETL {
    static func transform(_ old: OldType) -> NewType {
        var new: NewType = [:]
        for (key, values) in old {
            values.forEach { new[$0.lowercased()] = key }
        }
        return new
    }
}
