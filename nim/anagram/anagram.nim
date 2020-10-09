from algorithm import sorted
from sequtils  import filterIt
from strutils  import toLowerAscii

proc anagramKey(word: string): seq[char] =
  word.toLowerAscii.sorted

proc sameWord(a: string, b: string): bool =
  a.toLowerAscii == b.toLowerAscii


proc detectAnagrams*(word: string, candidates: openArray[string]): seq[string] =
  let wordKey = word.anagramKey
  candidates
    .filterIt(it.anagramKey == wordKey)
    .filterIt(not it.sameWord word)
