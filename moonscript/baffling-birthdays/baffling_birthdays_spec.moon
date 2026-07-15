BafflingBirthdays = require 'baffling_birthdays'

-- ----------------------------------------
-- https://lunarmodules.github.io/Penlight/libraries/pl.tablex.html
tablex = require 'pl.tablex'

isLeapYear = (year) -> year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)
-- ----------------------------------------

describe 'baffling-birthdays:', ->
  describe 'shared birthday:', ->
    it 'one birthdate', ->
      birthdates = {'2000-01-01'}
      assert.is.false BafflingBirthdays.sharedBirthday birthdates

    it 'two birthdates with same year, month, and day', ->
      birthdates = {'2000-01-01', '2000-01-01'}
      assert.is.true BafflingBirthdays.sharedBirthday birthdates

    it 'two birthdates with same year and month, but different day', ->
      birthdates = {'2012-05-09', '2012-05-17'}
      assert.is.false BafflingBirthdays.sharedBirthday birthdates

    it 'two birthdates with same month and day, but different year', ->
      birthdates = {'1999-10-23', '1988-10-23'}
      assert.is.true BafflingBirthdays.sharedBirthday birthdates

    it 'two birthdates with same year, but different month and day', ->
      birthdates = {'2007-12-19', '2007-04-27'}
      assert.is.false BafflingBirthdays.sharedBirthday birthdates

    it 'two birthdates with different year, month, and day', ->
      birthdates = {'1997-08-04', '1963-11-23'}
      assert.is.false BafflingBirthdays.sharedBirthday birthdates

    it 'multiple birthdates without shared birthday', ->
      birthdates = {'1966-07-29', '1977-02-12', '2001-12-25', '1980-11-10'}
      assert.is.false BafflingBirthdays.sharedBirthday birthdates

    it 'multiple birthdates with one shared birthday', ->
      birthdates = {'1966-07-29', '1977-02-12', '2001-07-29', '1980-11-10'}
      assert.is.true BafflingBirthdays.sharedBirthday birthdates

    it 'multiple birthdates with more than one shared birthday', ->
      birthdates = {'1966-07-29', '1977-02-12', '2001-12-25', '1980-07-29', '2019-02-12'}
      assert.is.true BafflingBirthdays.sharedBirthday birthdates

  describe 'random birthdates:', ->
    it 'generate requested number of birthdates', ->
      result = true
      for n = 1, 100
        birthdates = BafflingBirthdays.randomBirthdates n
        result = result and (#birthdates == n)
      assert.is.true result

    it 'years are not leap years', ->
      result = true
      for birthdate in *BafflingBirthdays.randomBirthdates(100)
        year = birthdate\sub 1, 4
        result = result and not isLeapYear tonumber(year)
      assert.is.true result

    it 'months are random', ->
      months = tablex.new 12, 0
      for birthdate in *BafflingBirthdays.randomBirthdates(100)
        month = tonumber birthdate\sub 6, 7
        assert.is.true 1 <= month and month <= 12
        months[month] += 1
      notSeen = [month for month,count in pairs months when count == 0]
      assert.is.equal 0, #notSeen

    it 'days are random', ->
      days = tablex.new 31, 0
      for birthdate in *BafflingBirthdays.randomBirthdates(300)
        day = tonumber birthdate\sub 9, 10
        assert.is.true 1 <= day and day <= 31
        days[day] += 1
      notSeen = [day for day,count in pairs days when count == 0]
      assert.is.equal 0, #notSeen

  describe 'estimated probability of at least one shared birthday:', ->
    it 'for one person', ->
      result = BafflingBirthdays.estimatedProbabilityOfSharedBirthday 1
      assert.is.near 0.0, result, 1.0

    it 'among ten people', ->
      result = BafflingBirthdays.estimatedProbabilityOfSharedBirthday 10
      assert.is.near 11.694818, result, 1.0

    it 'among twenty-three people', ->
      result = BafflingBirthdays.estimatedProbabilityOfSharedBirthday 23
      assert.is.near 50.729723, result, 1.0

    it 'among seventy people', ->
      result = BafflingBirthdays.estimatedProbabilityOfSharedBirthday 70
      assert.is.near 99.915958, result, 1.0

