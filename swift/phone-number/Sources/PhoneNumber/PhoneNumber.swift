/* working with Swift regular expressions tutorials
https://code.tutsplus.com/tutorials/swift-and-regular-expressions-swift--cms-26626
https://nshipster.com/swift-regular-expressions/
*/

import Foundation

extension String {
    subscript(location: Int, length: Int) -> String {
        return String(
            self.prefix(location + length)
                .suffix(length)
        )
    }

    subscript(r: NSRange) -> String {
        return self[r.location, r.length]
    }
}

struct PhoneNumber: CustomStringConvertible {
    static let pattern = "^1?([2-9]\\d\\d)([2-9]\\d\\d)(\\d{4})$"
    static let regex = try! NSRegularExpression(pattern: pattern)

    private(set) var number:      String = "0000000000"
    private(set) var areaCode:    String = "000"
    private(set) var description: String = "(000) 000-0000"

    init(_ num: String) {
        var digits = num
        digits.removeAll { !( $0.isASCII && $0.isWholeNumber ) }

        PhoneNumber.regex.enumerateMatches(
            in: digits, 
            range: NSMakeRange(0, digits.count)
        ) {
            (match, _, stop) in
            self.areaCode = digits[ match!.range(at: 1) ]
            let exchange  = digits[ match!.range(at: 2) ]
            let lineNum   = digits[ match!.range(at: 3) ]
            self.number   = self.areaCode + exchange + lineNum
            self.description =
                "(\(self.areaCode)) \(exchange)-\(lineNum)"

            // only expect one match as the pattern is anchored
            stop.pointee = true
        }
    }
}