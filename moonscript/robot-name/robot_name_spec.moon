import table_size from require './test_helpers'

Robot = require 'robot_name'

describe 'robot-name', ->
  describe 'one robot', ->
    it 'a robot has a name', ->
      robot = Robot!
      name = robot\name!
      assert.is.match name, '^[A-Z][A-Z][0-9][0-9][0-9]$'

    it 'name does not change', ->
      robot = Robot!
      name1 = robot\name!
      name2 = robot\name!
      assert.are.equal name1, name2

    it 'reset changes name', ->
      robot = Robot!
      name1 = robot\name!
      robot\reset!
      name2 = robot\name!
      assert.not.equal name1, name2

    it 'reset before name called does not cause an error', ->
      f = ->
        robot = Robot!
        robot\reset!
        robot\name!
      assert.has_no.errors f

  describe 'two robots', ->
    it 'different robots have different names', ->
      r1 = Robot!
      r2 = Robot!
      assert.not.equal r1\name!, r2\name!

    it 'names must be random not consecutive', ->
      Robot\reset_names!
      diffs = {}
      for _ = 1, 10
        n1 = Robot!\name!
        n2 = Robot!\name!
        -- a robot name is a valid base-36 number
        diff = tonumber(n1, 36) - tonumber(n2, 36)
        diffs[diff] = true
      assert.not.equal 1, table_size diffs

  describe 'lots of robots', ->
    it 'a large number of robots have unique names', ->
      sample_size = 10000
      seen = {}
      for i = 1, sample_size
        name = Robot!\name!
        seen[name] = true
      assert.is.equal sample_size, table_size seen


    -- The next test is optional.
    -- Set the environment variable ENABLE_OPTIONAL_TESTS to run it:
    -- For example, in bash run:  ENABLE_OPTIONAL_TESTS=true busted
    -- Check the performance tip in the hints.

    if os.getenv('ENABLE_OPTIONAL_TESTS') == 'true'
      it 'all the robots and more', ->
        Robot\reset_names!
        sample_size = 26 * 26 * 1000
        seen = {}
        for i = 1, sample_size
          name = Robot!\name!
          seen[name] = true
        assert.is.equal sample_size, table_size seen
        -- the 676,001st robot
        f = -> Robot!
        assert.has_error f, 'all names taken'
