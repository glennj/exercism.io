class Matrix
  parseLine = (line) -> line.split(' ').map((n) -> Number.parseInt(n))

  constructor: (input) -> @rows = (parseLine line for line in input.split("\n"))

  row: (index) -> @rows[index - 1]

  column: (index) -> @rows.map((row) -> row[index - 1])

module.exports = Matrix
