import sequtils, strutils

proc matrix(input: string): seq[seq[int]] =
  input
    .splitLines
    .mapIt( it.splitWhitespace.map(parseInt) )

proc row*(input: string, row: int): seq[int] =
  matrix(input)[row - 1]

proc column*(input: string, col: int): seq[int] =
  matrix(input).mapIt(it[col - 1])
