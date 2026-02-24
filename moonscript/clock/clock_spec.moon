Clock = require 'clock'

describe 'clock', ->
  describe 'Create a new clock with an initial time', ->
    it 'on the hour', ->
      clock = Clock hour: 8, minute: 0
      assert.are.equal '08:00', tostring clock

    it 'past the hour', ->
      clock = Clock hour: 11, minute: 9
      assert.are.equal '11:09', tostring clock

    it 'midnight is zero hours', ->
      clock = Clock hour: 24, minute: 0
      assert.are.equal '00:00', tostring clock

    it 'hour rolls over', ->
      clock = Clock hour: 25, minute: 0
      assert.are.equal '01:00', tostring clock

    it 'hour rolls over continuously', ->
      clock = Clock hour: 100, minute: 0
      assert.are.equal '04:00', tostring clock

    it 'sixty minutes is next hour', ->
      clock = Clock hour: 1, minute: 60
      assert.are.equal '02:00', tostring clock

    it 'minutes roll over', ->
      clock = Clock hour: 0, minute: 160
      assert.are.equal '02:40', tostring clock

    it 'minutes roll over continuously', ->
      clock = Clock hour: 0, minute: 1723
      assert.are.equal '04:43', tostring clock

    it 'hour and minutes roll over', ->
      clock = Clock hour: 25, minute: 160
      assert.are.equal '03:40', tostring clock

    it 'hour and minutes roll over continuously', ->
      clock = Clock hour: 201, minute: 3001
      assert.are.equal '11:01', tostring clock

    it 'hour and minutes roll over to exactly midnight', ->
      clock = Clock hour: 72, minute: 8640
      assert.are.equal '00:00', tostring clock

    it 'negative hour', ->
      clock = Clock hour: -1, minute: 15
      assert.are.equal '23:15', tostring clock

    it 'negative hour rolls over', ->
      clock = Clock hour: -25, minute: 0
      assert.are.equal '23:00', tostring clock

    it 'negative hour rolls over continuously', ->
      clock = Clock hour: -91, minute: 0
      assert.are.equal '05:00', tostring clock

    it 'negative minutes', ->
      clock = Clock hour: 1, minute: -40
      assert.are.equal '00:20', tostring clock

    it 'negative minutes roll over', ->
      clock = Clock hour: 1, minute: -160
      assert.are.equal '22:20', tostring clock

    it 'negative minutes roll over continuously', ->
      clock = Clock hour: 1, minute: -4820
      assert.are.equal '16:40', tostring clock

    it 'negative sixty minutes is previous hour', ->
      clock = Clock hour: 2, minute: -60
      assert.are.equal '01:00', tostring clock

    it 'negative hour and minutes both roll over', ->
      clock = Clock hour: -25, minute: -160
      assert.are.equal '20:20', tostring clock

    it 'negative hour and minutes both roll over continuously', ->
      clock = Clock hour: -121, minute: -5810
      assert.are.equal '22:10', tostring clock

  describe 'Add minutes', ->
    it 'add minutes', ->
      clock = Clock hour: 10, minute: 0
      clock\add 3
      assert.are.equal '10:03', tostring clock

    it 'add no minutes', ->
      clock = Clock hour: 6, minute: 41
      clock\add 0
      assert.are.equal '06:41', tostring clock

    it 'add to next hour', ->
      clock = Clock hour: 0, minute: 45
      clock\add 40
      assert.are.equal '01:25', tostring clock

    it 'add more than one hour', ->
      clock = Clock hour: 10, minute: 0
      clock\add 61
      assert.are.equal '11:01', tostring clock

    it 'add more than two hours with carry', ->
      clock = Clock hour: 0, minute: 45
      clock\add 160
      assert.are.equal '03:25', tostring clock

    it 'add across midnight', ->
      clock = Clock hour: 23, minute: 59
      clock\add 2
      assert.are.equal '00:01', tostring clock

    it 'add more than one day (1500 min = 25 hrs)', ->
      clock = Clock hour: 5, minute: 32
      clock\add 1500
      assert.are.equal '06:32', tostring clock

    it 'add more than two days', ->
      clock = Clock hour: 1, minute: 1
      clock\add 3500
      assert.are.equal '11:21', tostring clock

  describe 'Subtract minutes', ->
    it 'subtract minutes', ->
      clock = Clock hour: 10, minute: 3
      clock\subtract 3
      assert.are.equal '10:00', tostring clock

    it 'subtract to previous hour', ->
      clock = Clock hour: 10, minute: 3
      clock\subtract 30
      assert.are.equal '09:33', tostring clock

    it 'subtract more than an hour', ->
      clock = Clock hour: 10, minute: 3
      clock\subtract 70
      assert.are.equal '08:53', tostring clock

    it 'subtract across midnight', ->
      clock = Clock hour: 0, minute: 3
      clock\subtract 4
      assert.are.equal '23:59', tostring clock

    it 'subtract more than two hours', ->
      clock = Clock hour: 0, minute: 0
      clock\subtract 160
      assert.are.equal '21:20', tostring clock

    it 'subtract more than two hours with borrow', ->
      clock = Clock hour: 6, minute: 15
      clock\subtract 160
      assert.are.equal '03:35', tostring clock

    it 'subtract more than one day (1500 min = 25 hrs)', ->
      clock = Clock hour: 5, minute: 32
      clock\subtract 1500
      assert.are.equal '04:32', tostring clock

    it 'subtract more than two days', ->
      clock = Clock hour: 2, minute: 20
      clock\subtract 3000
      assert.are.equal '00:20', tostring clock

  describe 'Compare two clocks for equality', ->
    it 'clocks with same time', ->
      a = Clock hour: 15, minute: 37
      b = Clock hour: 15, minute: 37
      assert.is_true, a\equals(b)

    it 'clocks a minute apart', ->
      a = Clock hour: 15, minute: 36
      b = Clock hour: 15, minute: 37
      assert.is_false, a\equals(b)

    it 'clocks an hour apart', ->
      a = Clock hour: 14, minute: 37
      b = Clock hour: 15, minute: 37
      assert.is_false, a\equals(b)

    it 'clocks with hour overflow', ->
      a = Clock hour: 10, minute: 37
      b = Clock hour: 34, minute: 37
      assert.is_true, a\equals(b)

    it 'clocks with hour overflow by several days', ->
      a = Clock hour: 3, minute: 11
      b = Clock hour: 99, minute: 11
      assert.is_true, a\equals(b)

    it 'clocks with negative hour', ->
      a = Clock hour: 22, minute: 40
      b = Clock hour: -2, minute: 40
      assert.is_true, a\equals(b)

    it 'clocks with negative hour that wraps', ->
      a = Clock hour: 17, minute: 3
      b = Clock hour: -31, minute: 3
      assert.is_true, a\equals(b)

    it 'clocks with negative hour that wraps multiple times', ->
      a = Clock hour: 13, minute: 49
      b = Clock hour: -83, minute: 49
      assert.is_true, a\equals(b)

    it 'clocks with minute overflow', ->
      a = Clock hour: 0, minute: 1
      b = Clock hour: 0, minute: 1441
      assert.is_true, a\equals(b)

    it 'clocks with minute overflow by several days', ->
      a = Clock hour: 2, minute: 2
      b = Clock hour: 2, minute: 4322
      assert.is_true, a\equals(b)

    it 'clocks with negative minute', ->
      a = Clock hour: 2, minute: 40
      b = Clock hour: 3, minute: -20
      assert.is_true, a\equals(b)

    it 'clocks with negative minute that wraps', ->
      a = Clock hour: 4, minute: 10
      b = Clock hour: 5, minute: -1490
      assert.is_true, a\equals(b)

    it 'clocks with negative minute that wraps multiple times', ->
      a = Clock hour: 6, minute: 15
      b = Clock hour: 6, minute: -4305
      assert.is_true, a\equals(b)

    it 'clocks with negative hours and minutes', ->
      a = Clock hour: 7, minute: 32
      b = Clock hour: -12, minute: -268
      assert.is_true, a\equals(b)

    it 'clocks with negative hours and minutes that wrap', ->
      a = Clock hour: 18, minute: 7
      b = Clock hour: -54, minute: -11513
      assert.is_true, a\equals(b)

    it 'full clock and zeroed clock', ->
      a = Clock hour: 24, minute: 0
      b = Clock hour: 0, minute: 0
      assert.is_true, a\equals(b)
