import "./factorial" for Factorial

class PascalsTriangle {
  // 2-dimensional array approach
  static rows2D(count) {
    var rows = [[1], [1, 1]]
    if (count <= 2) return rows[0...count]
    for (i in 2...count) {
      var row = List.filled(i + 1, 1)
      for (j in 1...i) row[j] = rows[i-1][j-1] + rows[i-1][j]
      rows.add(row)
    }
    return rows
  }

  // mathematical approach
  static rows(count) {
    var f = Factorial.new()
    return (0...count).map {|i|
      return (0..i).map {|j| f.nChooseK(i, j)}.toList
    }.toList
  }
}
