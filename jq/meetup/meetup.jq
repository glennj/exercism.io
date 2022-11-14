#############################################################
# This approach first generates the entire month, creating an object
# keyed by the day of week ("Monday", ...) and the values are arrays
# of the month days for each weekday.
#
# Then the solution is to index the object by weekday, and then index
# the value by the wanted week number.
#############################################################

# Given an array of [year, month, day], return the "broken-down time"
def time_value: .[:3] | join("-") | strptime("%Y-%m-%d");

# Given a "time value", return the full day of week
def day_of_week: time_value | strftime("%A");

# Given a "time value", return the ISO date
def ymd: time_value | strftime("%Y-%m-%d");

# last day of given month:
# find the _first_ day of _next_ month then subtract 1 day
def last_day:
  if .month == 12
    then [.year + 1, 1, 1]
    else [.year, .month + 1, 1]
  end
  | time_value
  | mktime - 86400
  | strftime("%d")
  | tonumber
;

# Return an object where:
# - the keys are days of the week and 
# - values are arrays of month days for that weekday
#
# Example:
#   {year: 2022, month: 8} | month_days
#   => {
#        "Friday":[5,12,19,26],
#        "Monday":[1,8,15,22,29],
#        "Saturday":[6,13,20,27],
#        "Sunday":[7,14,21,28],
#        "Thursday":[4,11,18,25],
#        "Tuesday":[2,9,16,23,30],
#        "Wednesday":[3,10,17,24,31]
#      }
#
def month_days:
  [.year, .month] as [$y, $m]
  | [
      range(1; last_day + 1)
      | [$y, $m, .]
      | {day: last, weekday: day_of_week}
    ]
  | group_by(.weekday)
  | map({key: (first | .weekday), value: map(.day)})
  | from_entries
;

def wanted_day_of_month:
  [.week, .dayofweek]  as [$week, $dayofweek]
  | month_days
  | .[$dayofweek]
  | if   $week == "first"  then first
    elif $week == "last"   then last
    elif $week == "second" then nth(1)
    elif $week == "third"  then nth(2)
    elif $week == "fourth" then nth(3)
    elif $week == "teenth" then .[] | select(IN(range(13;20)))
    else "unknown 'week' value \($week)" | halt_error
    end
;

#############################################################
[.year, .month, wanted_day_of_month] | ymd
