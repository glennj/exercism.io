import meetup from require 'meetup'

describe 'meetup', ->
  it 'when teenth Monday is the 13th, the first day of the teenth week', ->
    result = meetup {year: 2013, month: 5, week: "teenth", dayofweek: "Monday"}
    expected = '2013-05-13'
    assert.are.equal expected, result

  it 'when teenth Monday is the 19th, the last day of the teenth week', ->
    result = meetup {year: 2013, month: 8, week: "teenth", dayofweek: "Monday"}
    expected = '2013-08-19'
    assert.are.equal expected, result

  it 'when teenth Monday is some day in the middle of the teenth week', ->
    result = meetup {year: 2013, month: 9, week: "teenth", dayofweek: "Monday"}
    expected = '2013-09-16'
    assert.are.equal expected, result

  it 'when teenth Tuesday is the 19th, the last day of the teenth week', ->
    result = meetup {year: 2013, month: 3, week: "teenth", dayofweek: "Tuesday"}
    expected = '2013-03-19'
    assert.are.equal expected, result

  it 'when teenth Tuesday is some day in the middle of the teenth week', ->
    result = meetup {year: 2013, month: 4, week: "teenth", dayofweek: "Tuesday"}
    expected = '2013-04-16'
    assert.are.equal expected, result

  it 'when teenth Tuesday is the 13th, the first day of the teenth week', ->
    result = meetup {year: 2013, month: 8, week: "teenth", dayofweek: "Tuesday"}
    expected = '2013-08-13'
    assert.are.equal expected, result

  it 'when teenth Wednesday is some day in the middle of the teenth week', ->
    result = meetup {year: 2013, month: 1, week: "teenth", dayofweek: "Wednesday"}
    expected = '2013-01-16'
    assert.are.equal expected, result

  it 'when teenth Wednesday is the 13th, the first day of the teenth week', ->
    result = meetup {year: 2013, month: 2, week: "teenth", dayofweek: "Wednesday"}
    expected = '2013-02-13'
    assert.are.equal expected, result

  it 'when teenth Wednesday is the 19th, the last day of the teenth week', ->
    result = meetup {year: 2013, month: 6, week: "teenth", dayofweek: "Wednesday"}
    expected = '2013-06-19'
    assert.are.equal expected, result

  it 'when teenth Thursday is some day in the middle of the teenth week', ->
    result = meetup {year: 2013, month: 5, week: "teenth", dayofweek: "Thursday"}
    expected = '2013-05-16'
    assert.are.equal expected, result

  it 'when teenth Thursday is the 13th, the first day of the teenth week', ->
    result = meetup {year: 2013, month: 6, week: "teenth", dayofweek: "Thursday"}
    expected = '2013-06-13'
    assert.are.equal expected, result

  it 'when teenth Thursday is the 19th, the last day of the teenth week', ->
    result = meetup {year: 2013, month: 9, week: "teenth", dayofweek: "Thursday"}
    expected = '2013-09-19'
    assert.are.equal expected, result

  it 'when teenth Friday is the 19th, the last day of the teenth week', ->
    result = meetup {year: 2013, month: 4, week: "teenth", dayofweek: "Friday"}
    expected = '2013-04-19'
    assert.are.equal expected, result

  it 'when teenth Friday is some day in the middle of the teenth week', ->
    result = meetup {year: 2013, month: 8, week: "teenth", dayofweek: "Friday"}
    expected = '2013-08-16'
    assert.are.equal expected, result

  it 'when teenth Friday is the 13th, the first day of the teenth week', ->
    result = meetup {year: 2013, month: 9, week: "teenth", dayofweek: "Friday"}
    expected = '2013-09-13'
    assert.are.equal expected, result

  it 'when teenth Saturday is some day in the middle of the teenth week', ->
    result = meetup {year: 2013, month: 2, week: "teenth", dayofweek: "Saturday"}
    expected = '2013-02-16'
    assert.are.equal expected, result

  it 'when teenth Saturday is the 13th, the first day of the teenth week', ->
    result = meetup {year: 2013, month: 4, week: "teenth", dayofweek: "Saturday"}
    expected = '2013-04-13'
    assert.are.equal expected, result

  it 'when teenth Saturday is the 19th, the last day of the teenth week', ->
    result = meetup {year: 2013, month: 10, week: "teenth", dayofweek: "Saturday"}
    expected = '2013-10-19'
    assert.are.equal expected, result

  it 'when teenth Sunday is the 19th, the last day of the teenth week', ->
    result = meetup {year: 2013, month: 5, week: "teenth", dayofweek: "Sunday"}
    expected = '2013-05-19'
    assert.are.equal expected, result

  it 'when teenth Sunday is some day in the middle of the teenth week', ->
    result = meetup {year: 2013, month: 6, week: "teenth", dayofweek: "Sunday"}
    expected = '2013-06-16'
    assert.are.equal expected, result

  it 'when teenth Sunday is the 13th, the first day of the teenth week', ->
    result = meetup {year: 2013, month: 10, week: "teenth", dayofweek: "Sunday"}
    expected = '2013-10-13'
    assert.are.equal expected, result

  it 'when first Monday is some day in the middle of the first week', ->
    result = meetup {year: 2013, month: 3, week: "first", dayofweek: "Monday"}
    expected = '2013-03-04'
    assert.are.equal expected, result

  it 'when first Monday is the 1st, the first day of the first week', ->
    result = meetup {year: 2013, month: 4, week: "first", dayofweek: "Monday"}
    expected = '2013-04-01'
    assert.are.equal expected, result

  it 'when first Tuesday is the 7th, the last day of the first week', ->
    result = meetup {year: 2013, month: 5, week: "first", dayofweek: "Tuesday"}
    expected = '2013-05-07'
    assert.are.equal expected, result

  it 'when first Tuesday is some day in the middle of the first week', ->
    result = meetup {year: 2013, month: 6, week: "first", dayofweek: "Tuesday"}
    expected = '2013-06-04'
    assert.are.equal expected, result

  it 'when first Wednesday is some day in the middle of the first week', ->
    result = meetup {year: 2013, month: 7, week: "first", dayofweek: "Wednesday"}
    expected = '2013-07-03'
    assert.are.equal expected, result

  it 'when first Wednesday is the 7th, the last day of the first week', ->
    result = meetup {year: 2013, month: 8, week: "first", dayofweek: "Wednesday"}
    expected = '2013-08-07'
    assert.are.equal expected, result

  it 'when first Thursday is some day in the middle of the first week', ->
    result = meetup {year: 2013, month: 9, week: "first", dayofweek: "Thursday"}
    expected = '2013-09-05'
    assert.are.equal expected, result

  it 'when first Thursday is another day in the middle of the first week', ->
    result = meetup {year: 2013, month: 10, week: "first", dayofweek: "Thursday"}
    expected = '2013-10-03'
    assert.are.equal expected, result

  it 'when first Friday is the 1st, the first day of the first week', ->
    result = meetup {year: 2013, month: 11, week: "first", dayofweek: "Friday"}
    expected = '2013-11-01'
    assert.are.equal expected, result

  it 'when first Friday is some day in the middle of the first week', ->
    result = meetup {year: 2013, month: 12, week: "first", dayofweek: "Friday"}
    expected = '2013-12-06'
    assert.are.equal expected, result

  it 'when first Saturday is some day in the middle of the first week', ->
    result = meetup {year: 2013, month: 1, week: "first", dayofweek: "Saturday"}
    expected = '2013-01-05'
    assert.are.equal expected, result

  it 'when first Saturday is another day in the middle of the first week', ->
    result = meetup {year: 2013, month: 2, week: "first", dayofweek: "Saturday"}
    expected = '2013-02-02'
    assert.are.equal expected, result

  it 'when first Sunday is some day in the middle of the first week', ->
    result = meetup {year: 2013, month: 3, week: "first", dayofweek: "Sunday"}
    expected = '2013-03-03'
    assert.are.equal expected, result

  it 'when first Sunday is the 7th, the last day of the first week', ->
    result = meetup {year: 2013, month: 4, week: "first", dayofweek: "Sunday"}
    expected = '2013-04-07'
    assert.are.equal expected, result

  it 'when second Monday is some day in the middle of the second week', ->
    result = meetup {year: 2013, month: 3, week: "second", dayofweek: "Monday"}
    expected = '2013-03-11'
    assert.are.equal expected, result

  it 'when second Monday is the 8th, the first day of the second week', ->
    result = meetup {year: 2013, month: 4, week: "second", dayofweek: "Monday"}
    expected = '2013-04-08'
    assert.are.equal expected, result

  it 'when second Tuesday is the 14th, the last day of the second week', ->
    result = meetup {year: 2013, month: 5, week: "second", dayofweek: "Tuesday"}
    expected = '2013-05-14'
    assert.are.equal expected, result

  it 'when second Tuesday is some day in the middle of the second week', ->
    result = meetup {year: 2013, month: 6, week: "second", dayofweek: "Tuesday"}
    expected = '2013-06-11'
    assert.are.equal expected, result

  it 'when second Wednesday is some day in the middle of the second week', ->
    result = meetup {year: 2013, month: 7, week: "second", dayofweek: "Wednesday"}
    expected = '2013-07-10'
    assert.are.equal expected, result

  it 'when second Wednesday is the 14th, the last day of the second week', ->
    result = meetup {year: 2013, month: 8, week: "second", dayofweek: "Wednesday"}
    expected = '2013-08-14'
    assert.are.equal expected, result

  it 'when second Thursday is some day in the middle of the second week', ->
    result = meetup {year: 2013, month: 9, week: "second", dayofweek: "Thursday"}
    expected = '2013-09-12'
    assert.are.equal expected, result

  it 'when second Thursday is another day in the middle of the second week', ->
    result = meetup {year: 2013, month: 10, week: "second", dayofweek: "Thursday"}
    expected = '2013-10-10'
    assert.are.equal expected, result

  it 'when second Friday is the 8th, the first day of the second week', ->
    result = meetup {year: 2013, month: 11, week: "second", dayofweek: "Friday"}
    expected = '2013-11-08'
    assert.are.equal expected, result

  it 'when second Friday is some day in the middle of the second week', ->
    result = meetup {year: 2013, month: 12, week: "second", dayofweek: "Friday"}
    expected = '2013-12-13'
    assert.are.equal expected, result

  it 'when second Saturday is some day in the middle of the second week', ->
    result = meetup {year: 2013, month: 1, week: "second", dayofweek: "Saturday"}
    expected = '2013-01-12'
    assert.are.equal expected, result

  it 'when second Saturday is another day in the middle of the second week', ->
    result = meetup {year: 2013, month: 2, week: "second", dayofweek: "Saturday"}
    expected = '2013-02-09'
    assert.are.equal expected, result

  it 'when second Sunday is some day in the middle of the second week', ->
    result = meetup {year: 2013, month: 3, week: "second", dayofweek: "Sunday"}
    expected = '2013-03-10'
    assert.are.equal expected, result

  it 'when second Sunday is the 14th, the last day of the second week', ->
    result = meetup {year: 2013, month: 4, week: "second", dayofweek: "Sunday"}
    expected = '2013-04-14'
    assert.are.equal expected, result

  it 'when third Monday is some day in the middle of the third week', ->
    result = meetup {year: 2013, month: 3, week: "third", dayofweek: "Monday"}
    expected = '2013-03-18'
    assert.are.equal expected, result

  it 'when third Monday is the 15th, the first day of the third week', ->
    result = meetup {year: 2013, month: 4, week: "third", dayofweek: "Monday"}
    expected = '2013-04-15'
    assert.are.equal expected, result

  it 'when third Tuesday is the 21st, the last day of the third week', ->
    result = meetup {year: 2013, month: 5, week: "third", dayofweek: "Tuesday"}
    expected = '2013-05-21'
    assert.are.equal expected, result

  it 'when third Tuesday is some day in the middle of the third week', ->
    result = meetup {year: 2013, month: 6, week: "third", dayofweek: "Tuesday"}
    expected = '2013-06-18'
    assert.are.equal expected, result

  it 'when third Wednesday is some day in the middle of the third week', ->
    result = meetup {year: 2013, month: 7, week: "third", dayofweek: "Wednesday"}
    expected = '2013-07-17'
    assert.are.equal expected, result

  it 'when third Wednesday is the 21st, the last day of the third week', ->
    result = meetup {year: 2013, month: 8, week: "third", dayofweek: "Wednesday"}
    expected = '2013-08-21'
    assert.are.equal expected, result

  it 'when third Thursday is some day in the middle of the third week', ->
    result = meetup {year: 2013, month: 9, week: "third", dayofweek: "Thursday"}
    expected = '2013-09-19'
    assert.are.equal expected, result

  it 'when third Thursday is another day in the middle of the third week', ->
    result = meetup {year: 2013, month: 10, week: "third", dayofweek: "Thursday"}
    expected = '2013-10-17'
    assert.are.equal expected, result

  it 'when third Friday is the 15th, the first day of the third week', ->
    result = meetup {year: 2013, month: 11, week: "third", dayofweek: "Friday"}
    expected = '2013-11-15'
    assert.are.equal expected, result

  it 'when third Friday is some day in the middle of the third week', ->
    result = meetup {year: 2013, month: 12, week: "third", dayofweek: "Friday"}
    expected = '2013-12-20'
    assert.are.equal expected, result

  it 'when third Saturday is some day in the middle of the third week', ->
    result = meetup {year: 2013, month: 1, week: "third", dayofweek: "Saturday"}
    expected = '2013-01-19'
    assert.are.equal expected, result

  it 'when third Saturday is another day in the middle of the third week', ->
    result = meetup {year: 2013, month: 2, week: "third", dayofweek: "Saturday"}
    expected = '2013-02-16'
    assert.are.equal expected, result

  it 'when third Sunday is some day in the middle of the third week', ->
    result = meetup {year: 2013, month: 3, week: "third", dayofweek: "Sunday"}
    expected = '2013-03-17'
    assert.are.equal expected, result

  it 'when third Sunday is the 21st, the last day of the third week', ->
    result = meetup {year: 2013, month: 4, week: "third", dayofweek: "Sunday"}
    expected = '2013-04-21'
    assert.are.equal expected, result

  it 'when fourth Monday is some day in the middle of the fourth week', ->
    result = meetup {year: 2013, month: 3, week: "fourth", dayofweek: "Monday"}
    expected = '2013-03-25'
    assert.are.equal expected, result

  it 'when fourth Monday is the 22nd, the first day of the fourth week', ->
    result = meetup {year: 2013, month: 4, week: "fourth", dayofweek: "Monday"}
    expected = '2013-04-22'
    assert.are.equal expected, result

  it 'when fourth Tuesday is the 28th, the last day of the fourth week', ->
    result = meetup {year: 2013, month: 5, week: "fourth", dayofweek: "Tuesday"}
    expected = '2013-05-28'
    assert.are.equal expected, result

  it 'when fourth Tuesday is some day in the middle of the fourth week', ->
    result = meetup {year: 2013, month: 6, week: "fourth", dayofweek: "Tuesday"}
    expected = '2013-06-25'
    assert.are.equal expected, result

  it 'when fourth Wednesday is some day in the middle of the fourth week', ->
    result = meetup {year: 2013, month: 7, week: "fourth", dayofweek: "Wednesday"}
    expected = '2013-07-24'
    assert.are.equal expected, result

  it 'when fourth Wednesday is the 28th, the last day of the fourth week', ->
    result = meetup {year: 2013, month: 8, week: "fourth", dayofweek: "Wednesday"}
    expected = '2013-08-28'
    assert.are.equal expected, result

  it 'when fourth Thursday is some day in the middle of the fourth week', ->
    result = meetup {year: 2013, month: 9, week: "fourth", dayofweek: "Thursday"}
    expected = '2013-09-26'
    assert.are.equal expected, result

  it 'when fourth Thursday is another day in the middle of the fourth week', ->
    result = meetup {year: 2013, month: 10, week: "fourth", dayofweek: "Thursday"}
    expected = '2013-10-24'
    assert.are.equal expected, result

  it 'when fourth Friday is the 22nd, the first day of the fourth week', ->
    result = meetup {year: 2013, month: 11, week: "fourth", dayofweek: "Friday"}
    expected = '2013-11-22'
    assert.are.equal expected, result

  it 'when fourth Friday is some day in the middle of the fourth week', ->
    result = meetup {year: 2013, month: 12, week: "fourth", dayofweek: "Friday"}
    expected = '2013-12-27'
    assert.are.equal expected, result

  it 'when fourth Saturday is some day in the middle of the fourth week', ->
    result = meetup {year: 2013, month: 1, week: "fourth", dayofweek: "Saturday"}
    expected = '2013-01-26'
    assert.are.equal expected, result

  it 'when fourth Saturday is another day in the middle of the fourth week', ->
    result = meetup {year: 2013, month: 2, week: "fourth", dayofweek: "Saturday"}
    expected = '2013-02-23'
    assert.are.equal expected, result

  it 'when fourth Sunday is some day in the middle of the fourth week', ->
    result = meetup {year: 2013, month: 3, week: "fourth", dayofweek: "Sunday"}
    expected = '2013-03-24'
    assert.are.equal expected, result

  it 'when fourth Sunday is the 28th, the last day of the fourth week', ->
    result = meetup {year: 2013, month: 4, week: "fourth", dayofweek: "Sunday"}
    expected = '2013-04-28'
    assert.are.equal expected, result

  it 'last Monday in a month with four Mondays', ->
    result = meetup {year: 2013, month: 3, week: "last", dayofweek: "Monday"}
    expected = '2013-03-25'
    assert.are.equal expected, result

  it 'last Monday in a month with five Mondays', ->
    result = meetup {year: 2013, month: 4, week: "last", dayofweek: "Monday"}
    expected = '2013-04-29'
    assert.are.equal expected, result

  it 'last Tuesday in a month with four Tuesdays', ->
    result = meetup {year: 2013, month: 5, week: "last", dayofweek: "Tuesday"}
    expected = '2013-05-28'
    assert.are.equal expected, result

  it 'last Tuesday in another month with four Tuesdays', ->
    result = meetup {year: 2013, month: 6, week: "last", dayofweek: "Tuesday"}
    expected = '2013-06-25'
    assert.are.equal expected, result

  it 'last Wednesday in a month with five Wednesdays', ->
    result = meetup {year: 2013, month: 7, week: "last", dayofweek: "Wednesday"}
    expected = '2013-07-31'
    assert.are.equal expected, result

  it 'last Wednesday in a month with four Wednesdays', ->
    result = meetup {year: 2013, month: 8, week: "last", dayofweek: "Wednesday"}
    expected = '2013-08-28'
    assert.are.equal expected, result

  it 'last Thursday in a month with four Thursdays', ->
    result = meetup {year: 2013, month: 9, week: "last", dayofweek: "Thursday"}
    expected = '2013-09-26'
    assert.are.equal expected, result

  it 'last Thursday in a month with five Thursdays', ->
    result = meetup {year: 2013, month: 10, week: "last", dayofweek: "Thursday"}
    expected = '2013-10-31'
    assert.are.equal expected, result

  it 'last Friday in a month with five Fridays', ->
    result = meetup {year: 2013, month: 11, week: "last", dayofweek: "Friday"}
    expected = '2013-11-29'
    assert.are.equal expected, result

  it 'last Friday in a month with four Fridays', ->
    result = meetup {year: 2013, month: 12, week: "last", dayofweek: "Friday"}
    expected = '2013-12-27'
    assert.are.equal expected, result

  it 'last Saturday in a month with four Saturdays', ->
    result = meetup {year: 2013, month: 1, week: "last", dayofweek: "Saturday"}
    expected = '2013-01-26'
    assert.are.equal expected, result

  it 'last Saturday in another month with four Saturdays', ->
    result = meetup {year: 2013, month: 2, week: "last", dayofweek: "Saturday"}
    expected = '2013-02-23'
    assert.are.equal expected, result

  it 'last Sunday in a month with five Sundays', ->
    result = meetup {year: 2013, month: 3, week: "last", dayofweek: "Sunday"}
    expected = '2013-03-31'
    assert.are.equal expected, result

  it 'last Sunday in a month with four Sundays', ->
    result = meetup {year: 2013, month: 4, week: "last", dayofweek: "Sunday"}
    expected = '2013-04-28'
    assert.are.equal expected, result

  it 'when last Wednesday in February in a leap year is the 29th', ->
    result = meetup {year: 2012, month: 2, week: "last", dayofweek: "Wednesday"}
    expected = '2012-02-29'
    assert.are.equal expected, result

  it 'last Wednesday in December that is also the last day of the year', ->
    result = meetup {year: 2014, month: 12, week: "last", dayofweek: "Wednesday"}
    expected = '2014-12-31'
    assert.are.equal expected, result

  it 'when last Sunday in February in a non-leap year is not the 29th', ->
    result = meetup {year: 2015, month: 2, week: "last", dayofweek: "Sunday"}
    expected = '2015-02-22'
    assert.are.equal expected, result

  it 'when first Friday is the 7th, the last day of the first week', ->
    result = meetup {year: 2012, month: 12, week: "first", dayofweek: "Friday"}
    expected = '2012-12-07'
    assert.are.equal expected, result
