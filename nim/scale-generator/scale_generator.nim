import tables, strutils
import algorithm

const ChromaticSharp = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]
const ChromaticFlat =  ["A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab"]
const TonicsFlat = ["d", "g", "c", "f", "bb", "eb minor", "F", "Bb", "Eb", "Ab", "Db", "Gb major"]
const Intervals = {'m': 1, 'M': 2, 'A': 3}.toTable


proc toTitleAscii(s: string): string =
  s[0].toUpperAscii & s[1..^1].toLowerAscii


proc scale*(tonic: string, intervals: string = "mmmmmmmmmmmm"): seq[string] =
  let scale = if tonic in TonicsFlat: ChromaticFlat else: ChromaticSharp
  let i = scale.find(tonic.toTitleAscii)
  doAssert(i >= 0, "Tonic not found")

  let notes = scale.rotatedLeft(i)
  var idx = 0
  for interval in intervals:
    result.add notes[idx]
    idx.inc Intervals[interval]
