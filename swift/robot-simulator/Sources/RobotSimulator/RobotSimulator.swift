/* Why, oh why, does swift not have basic
 * trig functions in the standard library?
 */
import Accelerate
extension Double {
    func cos() -> Double {
        var out: [Double] = [0]
        var num: Int32 = 1
        vvcos(&out, [self], &num)
        return out.first!
    }
    func sin() -> Double {
        var out: [Double] = [0]
        var num: Int32 = 1
        vvsin(&out, [self], &num)
        return out.first!
    }
}

// Handling direction and orientation

enum Direction: Int {
    case east  =   0
    case north =  90
    case west  = 180
    case south = 270

    init?(_ degrees: Int) {
        self.init(rawValue: degrees)
    }

    func asRadians() -> Double {
        return Double(self.rawValue) * Double.pi * 2 / 360
    }
}

protocol Orientable {
    var bearing: Direction {get set}
    mutating func orient(_ direction: Direction)
    mutating func turnLeft(_ amount: Int)
    mutating func turnRight()
}

extension Orientable {
    mutating func orient(_ direction: Direction) -> Void {
        self.bearing = direction
    }

    mutating func turnLeft(_ amount: Int = 90) -> Void {
        let newDir: Int = (self.bearing.rawValue + amount) % 360
        self.orient( Direction(newDir)! )
    }

    mutating func turnRight() -> Void {
        self.turnLeft(270)
    }
}

// Handling coordinates and moving

protocol Locatable: Orientable {
    var x: Int {get set}
    var y: Int {get set}
    var coordinates: [Int] {get}
    mutating func at(x: Int, y: Int)
    mutating func place(x: Int, y: Int, direction: Direction)
    mutating func advance()
}

extension Locatable {
    var coordinates: [Int] {
        get {return [self.x, self.y]}
    }

    mutating func at(x: Int, y: Int) -> Void {
        self.x = x
        self.y = y
    }

    mutating func place(x: Int, y: Int, direction: Direction) -> Void {
        self.at(x: x, y: y)
        self.orient(direction)
    }

    mutating func advance() -> Void {
        let dx = Int(self.bearing.asRadians().cos())
        let dy = Int(self.bearing.asRadians().sin())
        self.at( x: self.x + dx, y: self.y + dy )
    }
}

// Handling instructions

enum Command: Character {
    case turnLeft  = "L"
    case turnRight = "R"
    case advance   = "A"

    init?(_ rawValue: Character) {
        self.init(rawValue: rawValue)
    }
}

protocol Scriptable: Locatable {
    func instructions(_ script: String) -> [Command]
    mutating func evaluate(_ script: String)
}

extension Scriptable {
    func instructions(_ script: String) -> [Command] {
        return script.compactMap { Command($0) }
    }

    mutating func evaluate(_ script: String) -> Void {
        instructions(script).forEach {
            switch $0 {
                case .turnLeft:  self.turnLeft()
                case .turnRight: self.turnRight()
                case .advance:   self.advance()
            }
        }
    }
}

// The Robot

class SimulatedRobot: Scriptable {
    var x: Int = 0
    var y: Int = 0
    var bearing: Direction = .north
}
