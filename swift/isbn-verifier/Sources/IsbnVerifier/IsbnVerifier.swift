import Foundation

func isValid(_ input: String) -> Bool {
    var isbn = input
    isbn.removeAll { !($0.isWholeNumber || $0 == "X") }

    // validate 
    let regex = try! NSRegularExpression(pattern: "^\\d{9}[\\dX]$")
    let range = NSRange(location: 0, length: isbn.count)
    guard 1 == regex.numberOfMatches(in: isbn, range: range)
          else {return false}

    // split into digits
    var digits = isbn.prefix(9).compactMap { $0.wholeNumberValue! }
    // add check digit: "X" == 10
    digits.append(isbn.last!.wholeNumberValue ?? 10)

    let isbnSum = digits
        .enumerated()
        .map {(i, d) in d * (10 - i)}
        .reduce(0, +)
    return isbnSum % 11 == 0
}