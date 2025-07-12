class Matrix {
  static row(string, index) { new(string).row(index) }
  static column(string, index) { new(string).column(index) }

  construct new(string) {
    _matrix = string
      .split("\n")
      .map {|row| row.split(" ").map {|n| Num.fromString(n)}.toList}
      .toList
  }

  row(n) { _matrix[n - 1] }
  column(n) { _matrix.map {|row| row[n - 1]}.toList }
}
