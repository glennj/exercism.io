# the values in this object represent the _first day_ of the given week,
# except for Last which will be calculated
Weeks = {First: 1, Second: 8, Teenth: 13, Third: 15, Fourth: 22, Last: -99}

# these values correspond to the Javascript day values (Sun-Sat => 0-6)
Weekdays = {Sunday: 0, Monday: 1, Tuesday: 2, Wednesday: 3, Thursday: 4, Friday: 5, Saturday: 6}

# note that the month of a Javascript Date is zero-based
meetup = (input) ->
  if input.week is Weeks.Last
    # the zero'th day of next month is the last day of this month
    lastDay = new Date input.year, input.month, 0
    startDate = lastDay.getDate() - 6
  else
    startDate = input.week

  date = new Date input.year, input.month - 1, startDate
  # this should loop max 6 times
  while date.getDay() isnt input.dayofweek
    date.setDate date.getDate() + 1
    
  date

module.exports = {Weeks, Weekdays, meetup}
