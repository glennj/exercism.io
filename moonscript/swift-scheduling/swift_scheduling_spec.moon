import delivery_date from require 'swift_scheduling'

describe 'swift-scheduling', ->
  it 'NOW translates to two hours later', ->
    result = delivery_date 'NOW', '2012-02-13T09:00:00'
    expected = '2012-02-13T11:00:00'
    assert.are.equal expected, result

  it 'ASAP before one in the afternoon translates to today at five in the afternoon', ->
    result = delivery_date 'ASAP', '1999-06-03T09:45:00'
    expected = '1999-06-03T17:00:00'
    assert.are.equal expected, result

  it 'ASAP at one in the afternoon translates to tomorrow at one in the afternoon', ->
    result = delivery_date 'ASAP', '2008-12-21T13:00:00'
    expected = '2008-12-22T13:00:00'
    assert.are.equal expected, result

  it 'ASAP after one in the afternoon translates to tomorrow at one in the afternoon', ->
    result = delivery_date 'ASAP', '2008-12-21T14:50:00'
    expected = '2008-12-22T13:00:00'
    assert.are.equal expected, result

  it 'EOW on Monday translates to Friday at five in the afternoon', ->
    result = delivery_date 'EOW', '2025-02-03T16:00:00'
    expected = '2025-02-07T17:00:00'
    assert.are.equal expected, result

  it 'EOW on Tuesday translates to Friday at five in the afternoon', ->
    result = delivery_date 'EOW', '1997-04-29T10:50:00'
    expected = '1997-05-02T17:00:00'
    assert.are.equal expected, result

  it 'EOW on Wednesday translates to Friday at five in the afternoon', ->
    result = delivery_date 'EOW', '2005-09-14T11:00:00'
    expected = '2005-09-16T17:00:00'
    assert.are.equal expected, result

  it 'EOW on Thursday translates to Sunday at eight in the evening', ->
    result = delivery_date 'EOW', '2011-05-19T08:30:00'
    expected = '2011-05-22T20:00:00'
    assert.are.equal expected, result

  it 'EOW on Friday translates to Sunday at eight in the evening', ->
    result = delivery_date 'EOW', '2022-08-05T14:00:00'
    expected = '2022-08-07T20:00:00'
    assert.are.equal expected, result

  it 'EOW translates to leap day', ->
    result = delivery_date 'EOW', '2008-02-25T10:30:00'
    expected = '2008-02-29T17:00:00'
    assert.are.equal expected, result

  it '2M before the second month of this year translates to the first workday of the second month of this year', ->
    result = delivery_date '2M', '2007-01-02T14:15:00'
    expected = '2007-02-01T08:00:00'
    assert.are.equal expected, result

  it '11M in the eleventh month translates to the first workday of the eleventh month of next year', ->
    result = delivery_date '11M', '2013-11-21T15:30:00'
    expected = '2014-11-03T08:00:00'
    assert.are.equal expected, result

  it '4M in the ninth month translates to the first workday of the fourth month of next year', ->
    result = delivery_date '4M', '2019-11-18T15:15:00'
    expected = '2020-04-01T08:00:00'
    assert.are.equal expected, result

  it 'Q1 in the first quarter translates to the last workday of the first quarter of this year', ->
    result = delivery_date 'Q1', '2003-01-01T10:45:00'
    expected = '2003-03-31T08:00:00'
    assert.are.equal expected, result

  it 'Q4 in the second quarter translates to the last workday of the fourth quarter of this year', ->
    result = delivery_date 'Q4', '2001-04-09T09:00:00'
    expected = '2001-12-31T08:00:00'
    assert.are.equal expected, result

  it 'Q3 in the fourth quarter translates to the last workday of the third quarter of next year', ->
    result = delivery_date 'Q3', '2022-10-06T11:00:00'
    expected = '2023-09-29T08:00:00'
    assert.are.equal expected, result
