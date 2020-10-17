# The strategy here is to pre-calculate all names, and then
# hand out the next one when a new name is needed.
# This will make the program startup slower but
# retrieving the 676_000th name will be a whole lot faster.

import strformat, random
import sugar

proc buildNames(): array[26 * 26 * 1000, string] =
  var i = 0
  for a in 'A'..'Z':
    for b in 'A'..'Z':
      for c in 0..999:
        result[i] = &"{a}{b}{c:03d}"
        i.inc

var CurrentIdx = -1
var RobotNames = buildNames()
randomize()
RobotNames.shuffle

proc nextName(): string =
  CurrentIdx.inc
  assert CurrentIdx < RobotNames.len, "All robot names consumed"
  RobotNames[CurrentIdx]


type
  Robot* = ref object
    name*: string

proc makeRobot*(): Robot =
  Robot(name: nextName())

proc reset*(robot: Robot) =
  robot.name = nextName()
