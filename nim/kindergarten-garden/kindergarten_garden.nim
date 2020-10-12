import re, sequtils, strutils

type
  Plant* = enum
    Clover = "C", Grass = "G", Radishes = "R", Violets = "V"

const Students = [
  "Alice", "Bob", "Charlie", "David", "Eve", "Fred",
  "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry",
]

proc plant(init: char): Plant = parseEnum[Plant]($init)

proc garden(input: string): seq[seq[Plant]] =
  var rows = input.splitLines.mapIt( it.findAll(re"..") )
  rows[0].zip(rows[1]).mapIt( (it[0] & it[1]).map(plant) )

proc plants*(input: string, student: string): seq[Plant] =
  var plots = garden input
  try:
    plots[Students.find student]
  except IndexError:
    raise newException(ValueError, "unknown student")
