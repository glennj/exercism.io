type
  ListClassification* = enum
    Sublist, Superlist, Equal, Unequal


# in the spirit of the exercise, implement array equality
proc listEquals(a, b: openArray[int]): bool =
  if a.len != b.len:
    return false
  for i in 0 ..< a.len:
    if a[i] != b[i]:
      return false
  true


# thanks to ErikSchierboom for this iterator
# https://exercism.io/tracks/nim/exercises/sublist/solutions/252056773c5f41bb9bc8c73d82ae55cf
iterator slices(a: openArray[int], size: int): seq[int] =
  for i in size .. a.len:
    yield a[i - size ..< i]

# "a" contains "b"?
proc listContains(a, b: openArray[int]): bool =
  if a.len >= b.len:
    for s in a.slices(b.len):
      if s.listEquals(b):
        return true
  false


proc sublist*(a, b: openArray[int]): ListClassification =
  if   a.listEquals(b):   Equal
  elif b.listContains(a): Sublist
  elif a.listContains(b): Superlist
  else:                   Unequal

