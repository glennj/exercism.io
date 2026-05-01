isLeap = (y) -> y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)

randomYear = ->
  year = 1900 + math.random(120)
  isLeap(year) and randomYear! or year

randomBirthdate = ->
  y = randomYear!
  -- os.time normalizes the month and day
  os.date '%Y-%m-%d', os.time {year: y, month: 1, day: math.random 365}

{
  -- is there at least one shared birthday among the given birthdates?
  sharedBirthday: (birthdates) ->
    seen = {}
    for d in *birthdates
      ym = d\sub(-5)
      return true if seen[ym]
      seen[ym] = true
    false

  -- generate a list of n random birthdates
  randomBirthdates: (n) -> [randomBirthdate! for _ = 1, n]

  -- determine the probability that there is a shared birthday amongst n people
  -- https://en.wikipedia.org/wiki/Birthday_problem#Approximations
  estimatedProbabilityOfSharedBirthday: (n) ->
    100 * (1 - math.exp(-n * (n - 1) / 730))
}
