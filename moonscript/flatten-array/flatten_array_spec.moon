import flatten from require 'flatten_array'

describe 'flatten-array', ->
  it 'empty', ->
    input = {}
    expected = {}
    assert.are.same expected, flatten input

  pending 'no nesting', ->
    input = {0, 1, 2}
    expected = {0, 1, 2}
    assert.are.same expected, flatten input

  pending 'flattens a nested array', ->
    input = {{{}}}
    expected = {}
    assert.are.same expected, flatten input

  pending 'flattens array with just integers present', ->
    input = {1, {2, 3, 4, 5, 6, 7}, 8}
    expected = {1, 2, 3, 4, 5, 6, 7, 8}
    assert.are.same expected, flatten input

  pending '5 level nesting', ->
    input = {0, 2, {{2, 3}, 8, 100, 4, {{{50}}}}, -2}
    expected = {0, 2, 2, 3, 8, 100, 4, 50, -2}
    assert.are.same expected, flatten input

  pending '6 level nesting', ->
    input = {1, {2, {{3}}, {4, {{5}}}, 6, 7}, 8}
    expected = {1, 2, 3, 4, 5, 6, 7, 8}
    assert.are.same expected, flatten input

  pending 'null values are omitted from the final result', ->
    input = {1, 2, "null"}
    expected = {1, 2}
    assert.are.same expected, flatten input

  pending 'consecutive null values at the front of the array are omitted from the final result', ->
    input = {"null", "null", 3}
    expected = {3}
    assert.are.same expected, flatten input

  pending 'consecutive null values in the middle of the array are omitted from the final result', ->
    input = {1, "null", "null", 4}
    expected = {1, 4}
    assert.are.same expected, flatten input

  pending '6 level nested array with null values', ->
    input = {0, 2, {{2, 3}, 8, {{100}}, "null", {{"null"}}}, -2}
    expected = {0, 2, 2, 3, 8, 100, -2}
    assert.are.same expected, flatten input

  pending 'all values in nested array are null', ->
    input = {"null", {{{"null"}}}, "null", "null", {{"null", "null"}, "null"}, "null"}
    expected = {}
    assert.are.same expected, flatten input
