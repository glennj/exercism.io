import Foundation

class Gigasecond {
    static let gigasecond: TimeInterval = 1e9

    // this is known as a Computed Property
    static let dateFormatter: DateFormatter = {
        let df = DateFormatter()
        df.locale     = Locale(identifier: "en_US_POSIX")
        df.dateFormat = "yyyy-MM-dd'T'HH:mm:ss"
        df.timeZone   = TimeZone(identifier: "UTC")
        return df       
    }()

    let description: String
    
    init?(from datestring: String) {
        let timestamp = Gigasecond.dateFormatter.date(from: datestring)
        if timestamp != nil {
            description = Gigasecond.dateFormatter.string(
                from: timestamp! + Gigasecond.gigasecond
            )
        } else {
            return nil
        }
    }
}

/* TODO 

study https://exercism.io/tracks/swift/exercises/gigasecond/solutions/1830c916258f4ca4a6b3aa0345d23a87
- lazy class member

*/
