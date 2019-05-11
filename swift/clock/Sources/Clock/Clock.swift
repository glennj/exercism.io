import Foundation

struct Clock: Equatable, CustomStringConvertible {
    private var hours: Int
    private var minutes: Int

    init(hours: Int = 0, minutes: Int = 0) {
        // normalize the incoming values
        var mins = hours * 60 + minutes
        while mins < 0 {
            mins += 1440
        }
        (self.hours, self.minutes) = (mins % 1440).quotientAndRemainder(dividingBy: 60)
    }

    var description: String {
        get {
            return String(
                format: "%02d:%02d",
                arguments: [self.hours, self.minutes]
            )
        }
    }

    static func ==(_ a: Clock, _ b: Clock) -> Bool {
        return a.description == b.description
    }

    func add(minutes: Int) -> Clock {
        return Clock(
            hours: self.hours,
            minutes: self.minutes + minutes
        )
    }

    func subtract(minutes: Int) -> Clock {
        return self.add(minutes: -minutes)
    }
}