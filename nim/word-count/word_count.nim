import tables, strutils, re

let wordRegex = re(r"\w+(?:[']\w+)?")

proc countWords*(phrase: string): CountTable[string] =
  phrase.toLowerAscii
        .findAll(wordRegex)
        .toCountTable
