from sequtils import foldl

proc char2int(c: char): int =
  case c
  of '0': 0
  of '1': 1
  else: raise newException(ValueError, "invalid binary number")
  
proc binary*(binstr: string): int =
  foldl(binstr, (a shl 1) + b.char2int(), 0)
