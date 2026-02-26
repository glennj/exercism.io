import make_change from require 'change'

describe 'change', ->
  it 'change for 1 cent', ->
    result = make_change 1, {1, 5, 10, 25}
    expected = {1}
    assert.are.same expected, result

  it 'single coin change', ->
    result = make_change 25, {1, 5, 10, 25, 100}
    expected = {25}
    assert.are.same expected, result

  it 'multiple coin change', ->
    result = make_change 15, {1, 5, 10, 25, 100}
    expected = {5, 10}
    assert.are.same expected, result

  it 'change with Lilliputian Coins', ->
    result = make_change 23, {1, 4, 15, 20, 50}
    expected = {4, 4, 15}
    assert.are.same expected, result

  it 'change with Lower Elbonia Coins', ->
    result = make_change 63, {1, 5, 10, 21, 25}
    expected = {21, 21, 21}
    assert.are.same expected, result

  it 'large target values', ->
    result = make_change 999, {1, 2, 5, 10, 20, 50, 100}
    expected = {2, 2, 5, 20, 20, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100}
    assert.are.same expected, result

  it 'possible change without unit coins available', ->
    result = make_change 21, {2, 5, 10, 20, 50}
    expected = {2, 2, 2, 5, 10}
    assert.are.same expected, result

  it 'another possible change without unit coins available', ->
    result = make_change 27, {4, 5}
    expected = {4, 4, 4, 5, 5, 5}
    assert.are.same expected, result

  it 'a greedy approach is not optimal', ->
    result = make_change 20, {1, 10, 11}
    expected = {10, 10}
    assert.are.same expected, result

  it 'no coins make 0 change', ->
    result = make_change 0, {1, 5, 10, 21, 25}
    expected = {}
    assert.are.same expected, result

  it 'error testing for change smaller than the smallest of coins', ->
    f = -> make_change 3, {5, 10}
    assert.has.error f, "can't make target with given coins"

  it 'error if no combination can add up to target', ->
    f = -> make_change 94, {5, 10}
    assert.has.error f, "can't make target with given coins"

  it 'cannot find negative change values', ->
    f = -> make_change -5, {1, 2, 5}
    assert.has.error f, "target can't be negative"
