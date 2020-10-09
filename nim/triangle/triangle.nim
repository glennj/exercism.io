import algorithm, sequtils
#import intsets

type 
  Triangle = array[3, int]

# test triangle inequality
proc valid(t: Triangle): bool =
  let ts = t.sorted
  (ts[2] < ts[0] + ts[1]) and t.allIt(it > 0)

#[ I reinvented sequtils:deduplicate :(
# count the number of distinct sides
proc uniq(t: Triangle): IntSet =
  for side in t:
    result.incl side
]#

proc test(t: Triangle, nUniq: openArray[int]): bool =
  t.valid and t.deduplicate.len in nUniq

proc isEquilateral*(t: Triangle): bool = t.test [1]
proc isIsosceles*  (t: Triangle): bool = t.test [1, 2]
proc isScalene*    (t: Triangle): bool = t.test [3]

