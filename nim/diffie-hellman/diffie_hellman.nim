import random
from math import `^`

randomize()

proc privateKey*(p: int): int =
  rand(2 ..< p)

proc publicKey*(p, g, privKey: int): int =
  g ^ privKey mod p

proc secret*(p, pubKey, privKey: int): int =
  publicKey(p, pubKey, privKey)

