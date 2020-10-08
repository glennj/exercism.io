#[
  My Clock object has a single inst var: minutes since
  midnight. That allows easier addition and subtraction
]#

import strformat

const MinsPerDay = 24 * 60

type
  HrsAndMins = tuple
    hours, minutes: int

  Clock = object
    value: int


proc hours(clock: Clock): int =
  clock.value div 60

proc minutes(clock: Clock): int =
  clock.value mod 60

proc toStr*(clock: Clock): string =
  &"{clock.hours:02d}:{clock.minutes:02d}"


# Normalize an (hours, minutes) tuple into number of minutes
# since midnight.  Handles negative values.
proc toMinutes(time: HrsAndMins): int =
  var mins = 60 * time.hours + time.minutes
  while mins < 0: mins.inc MinsPerDay
  mins mod MinsPerDay


proc create*(time: HrsAndMins): Clock =
  Clock(value: time.toMinutes)

proc add*(time: HrsAndMins, delta: int): Clock =
  create((0, time.toMinutes + delta))

proc subtract*(time: HrsAndMins, delta: int): Clock =
  add(time, -1 * delta)
