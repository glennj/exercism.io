from strutils import toUpperAscii

proc score*(word: string): int =
  for c in word.toUpperAscii:
    case c
    of 'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T':
      result.inc 1
    of 'D', 'G':
      result.inc 2
    of 'B', 'C', 'M', 'P':
      result.inc 3
    of 'F', 'H', 'V', 'W', 'Y':
      result.inc 4
    of 'K':
      result.inc 5
    of 'J', 'X':
      result.inc 8
    of 'Q', 'Z':
      result.inc 10
    else:
      discard
