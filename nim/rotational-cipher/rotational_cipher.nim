import sequtils, strutils, tables

proc mapping(n: int): Table[char, char] =
  var source = ('a'..'z').toSeq
  var dest = source[n..^1] & source[0..<n]

  source.add source.map(toUpperAscii)
  dest.add   dest.map(toUpperAscii)

  source.zip(dest).toTable


proc rotate*(input: string, amount: int): string =
  let map = mapping(amount)
  input
    .mapIt( map.getOrDefault(it, it) )
    .join("")
