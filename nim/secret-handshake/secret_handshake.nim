import algorithm

const Secrets = ["wink", "double blink", "close your eyes", "jump"]

# could use bitops:testBit instead
proc bitAt(n: int, idx: int): bool =
  (n and (1 shl idx)) != 0

proc commands*(code: int): seq[string] =
  for i in 0 ..< Secrets.len:
    if code.bitAt i:
      result.add Secrets[i]

  if code.bitAt Secrets.len:
    result.reverse
