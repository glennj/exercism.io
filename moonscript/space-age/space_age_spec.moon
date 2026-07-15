SpaceAge = require 'space_age'

describe 'space-age:', ->
  it 'age on Earth', ->
    result = SpaceAge.age 'Earth', 1000000000
    assert.is.near 31.69, result, 0.01

  it 'age on Mercury', ->
    result = SpaceAge.age 'Mercury', 2134835688
    assert.is.near 280.88, result, 0.01

  it 'age on Venus', ->
    result = SpaceAge.age 'Venus', 189839836
    assert.is.near 9.78, result, 0.01

  it 'age on Mars', ->
    result = SpaceAge.age 'Mars', 2129871239
    assert.is.near 35.88, result, 0.01

  it 'age on Jupiter', ->
    result = SpaceAge.age 'Jupiter', 901876382
    assert.is.near 2.41, result, 0.01

  it 'age on Saturn', ->
    result = SpaceAge.age 'Saturn', 2000000000
    assert.is.near 2.15, result, 0.01

  it 'age on Uranus', ->
    result = SpaceAge.age 'Uranus', 1210123456
    assert.is.near 0.46, result, 0.01

  it 'age on Neptune', ->
    result = SpaceAge.age 'Neptune', 1821023456
    assert.is.near 0.35, result, 0.01

  it 'invalid planet causes error', ->
    f = -> SpaceAge.age 'Sun', 680804807
    assert.has.errors f, 'not a planet'

