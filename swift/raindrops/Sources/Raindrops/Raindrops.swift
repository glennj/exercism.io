/****************************************** 
 * First take, not DRY enough for my taste

struct Raindrops {
    let sounds: String

    init(_ n: Int) {
        var sounds = ""
        if n % 3 == 0 { sounds += "Pling" }
        if n % 5 == 0 { sounds += "Plang" }
        if n % 7 == 0 { sounds += "Plong" }
        if sounds.isEmpty { sounds = String(n) }
        self.sounds = sounds
    }
}
*/

/****************************************** 
 * Second take, too heavy, even less DRY

enum Drops: Int, CaseIterable {
    case pling = 3
    case plang = 5
    case plong = 7 

    var sound: String {
        switch self {
            case .pling: return "Pling"
            case .plang: return "Plang"
            case .plong: return "Plong"
        }
    }
}

struct Raindrops {
    let sounds: String

    init(_ n: Int) {
        var sounds = ""
        for drop in Drops.allCases {
            if n % drop.rawValue == 0 {
                sounds += drop.sound
            }
        }
        self.sounds = sounds.isEmpty ? String(n) : sounds 
    }
}
*/

/****************************************** 
 * Third take, corresponding arrays

struct Raindrops {
    let sounds: String

    init(_ n: Int) {
        let dropValues = [3, 5, 7]
        let dropSounds = ["Pling", "Plang", "Plong"]
        var sounds = ""
        for (i, val) in dropValues.enumerated() {
            if n % val == 0 {
                sounds += dropSounds[i]
            }
        }
        self.sounds = sounds.isEmpty ? String(n) : sounds 
    }
}
*/

/****************************************** 
 * take 4, a more functional approach
 */



struct Raindrops {
    static let Sounds = ["Pling", "Plang", "Plong"]
    static let Values = [3, 5, 7]

    let n: Int
    init(_ n: Int) { self.n = n }

    var sounds: String {
        get {
            let sounds = Raindrops.Sounds
                .enumerated()
                .filter { self.n % Raindrops.Values[$0.0] == 0 }
                .map    { $0.1 }

            return sounds.isEmpty 
                ? String(self.n)
                : sounds.joined(separator: "")
        }
    }
}
