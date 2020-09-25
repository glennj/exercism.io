import sequtils

proc distance*(a: string, b: string): int =
  if (a.len != b.len):
    raise newException(ValueError, "must be same length")
  else:
    a.zip(b).filterIt( it[0] != it[1] ).len
