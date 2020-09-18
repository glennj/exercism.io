import strutils, sequtils

const alphabetSize = ('a'..'z').len

proc isPangram*(input: string): bool =
  alphabetSize == input.toLowerAscii
                       .filter(isAlphaAscii)
                       .deduplicate
                       .len
