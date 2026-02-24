SpaceAge = require 'space_age'

describe 'space-age', ->
  -- ----------------------------------------
  epsilon = 1e-2
  is_close_to = (state, arguments) ->
    {a, b} = arguments
    math.abs(a - b) <= epsilon

  say = require 'say'
  say\set 'assertion.approx_equal.positive', "Expected %s and %s to be within #{epsilon}"
  say\set 'assertion.approx_equal.negative', "Expected %s and %s not to be within #{epsilon}"
  assert\register 'assertion', 'approx_equal', is_close_to, 'assertion.approx_equal.positive', 'assertion.approx_equal.negative'
  -- ----------------------------------------

  it 'age on Earth', ->
    result = SpaceAge.age 'Earth', 1000000000
    assert.approx_equal 31.69, result

    -- Why do we need to test "approximately equal"?
    -- See https://0.30000000000000004.com

  it 'age on Mercury', ->
    result = SpaceAge.age 'Mercury', 2134835688
    assert.approx_equal 280.88, result

  it 'age on Venus', ->
    result = SpaceAge.age 'Venus', 189839836
    assert.approx_equal 9.78, result

  it 'age on Mars', ->
    result = SpaceAge.age 'Mars', 2129871239
    assert.approx_equal 35.88, result

  it 'age on Jupiter', ->
    result = SpaceAge.age 'Jupiter', 901876382
    assert.approx_equal 2.41, result

  it 'age on Saturn', ->
    result = SpaceAge.age 'Saturn', 2000000000
    assert.approx_equal 2.15, result

  it 'age on Uranus', ->
    result = SpaceAge.age 'Uranus', 1210123456
    assert.approx_equal 0.46, result

  it 'age on Neptune', ->
    result = SpaceAge.age 'Neptune', 1821023456
    assert.approx_equal 0.35, result

  it 'invalid planet causes error', ->
    f = -> SpaceAge.age 'Sun', 680804807
    assert.has.errors f, 'not a planet'
