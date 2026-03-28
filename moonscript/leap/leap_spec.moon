is_leap_year = require 'leap'

describe 'leap', ->
  it 'year not divisible by 4 in common year', ->
    assert.is_false is_leap_year 2015

  pending 'year divisible by 2, not divisible by 4 in common year', ->
    assert.is_false is_leap_year 1970

  pending 'year divisible by 4, not divisible by 100 in leap year', ->
    assert.is_true is_leap_year 1996

  pending 'year divisible by 4 and 5 is still a leap year', ->
    assert.is_true is_leap_year 1960

  pending 'year divisible by 100, not divisible by 400 in common year', ->
    assert.is_false is_leap_year 2100

  pending 'year divisible by 100 but not by 3 is still not a leap year', ->
    assert.is_false is_leap_year 1900

  pending 'year divisible by 400 is leap year', ->
    assert.is_true is_leap_year 2000

  pending 'year divisible by 400 but not by 125 is still a leap year', ->
    assert.is_true is_leap_year 2400

  pending 'year divisible by 200, not divisible by 400 in common year', ->
    assert.is_false is_leap_year 1800
