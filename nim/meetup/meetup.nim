import sequtils, times

type 
  Which* = enum
    First, Second, Third, Fourth, Last, Teenth

proc datetimesInMonth(year: int, month: Month): seq[DateTime] =
  for day in 1..getDaysInMonth(month, year):
    result.add initDateTime(day, month, year, 12, 0, 0)

proc findMeetup(monthDays: seq[DateTime], which: Which, weekDay: WeekDay): DateTime =
  var days = monthDays.filterIt(it.weekday == weekDay)
  case which
    of First, Second, Third, Fourth: days[which.ord]
    of Last:   days[^1]
    of Teenth: (days.filterIt(it.monthDay in 13..19))[0]

proc meetup*(year: int, month: int, which: Which, weekDay: WeekDay): string =
  datetimesInMonth(year, Month(month))
    .findMeetup(which, weekDay)
    .format("yyyy-MM-dd")
