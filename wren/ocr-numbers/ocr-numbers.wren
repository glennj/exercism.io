class OcrNumbers {
  static convert(lines) {
    if (lines.count % 4 != 0) Fiber.abort("Number of input lines is not a multiple of four")
    if (lines[0].count % 3 != 0) Fiber.abort("Number of input columns is not a multiple of three")

    var translation = {
      " _ | ||_|   ": "0",
      "     |  |   ": "1",
      " _  _||_    ": "2",
      " _  _| _|   ": "3",
      "   |_|  |   ": "4",
      " _ |_  _|   ": "5",
      " _ |_ |_|   ": "6",
      " _   |  |   ": "7",
      " _ |_||_|   ": "8",
      " _ |_| _|   ": "9"
    }

    var rows = []

    while (true) {
      var row = []
      var nChars = lines[0].count / 3
      for (i in 0...nChars) {
        var start = 3 * i
        var digit = (0...4).map {|j| lines[j][start...start+3]}.join("")
        row.add(translation.containsKey(digit) ? translation[digit] : "?")
      }
      rows.add(row.join(""))

      if (lines.count == 4) break
      lines = lines[4..-1]
    }

    return rows.join(",")
  }
}
