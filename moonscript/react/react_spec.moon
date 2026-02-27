import InputCell, ComputeCell from require 'react'

describe 'react', ->
  it 'input cells have a value', ->
    input = InputCell 2
    assert.are.equal 2, input\get_value!

  it "an input cell's value can be set", ->
    input = InputCell 4
    input\set_value 20
    assert.are.equal 20, input\get_value!

  it 'compute cells calculate initial value', ->
    input = InputCell 1
    output = ComputeCell input, (x) -> x + 1
    assert.are.equal 2, output\get_value!

  it 'compute cells take inputs in the right order', ->
    one = InputCell 1
    two = InputCell 2
    output = ComputeCell one, two, (x, y) -> x + y * 10
    assert.are.equal 21, output\get_value!

  it 'compute cells update value when dependencies are changed', ->
    input = InputCell 1
    output = ComputeCell input, (x) -> x + 1

    input\set_value 3
    assert.are.equal 4, output\get_value!

  it 'compute cells can depend on other compute cells', ->
    input = InputCell 1
    times_two = ComputeCell input, (x) -> x * 2
    times_thirty = ComputeCell input, (x) -> x * 30

    output = ComputeCell times_two, times_thirty, (x, y) -> x + y
    assert.are.equal 32, output\get_value!

    input\set_value 3
    assert.are.equal 96, output\get_value!

  it 'compute cells fire callbacks', ->
    input = InputCell 1
    output = ComputeCell input, (x) -> x + 1

    -- "Spies" are documented here: https://lunarmodules.github.io/busted/#spies-mocks-stubs
    callback = spy.new(->) -- the argument is an empty function

    output\watch callback
    input\set_value 3
    assert.spy(callback).was_called 1
    assert.spy(callback).was_called_with 4

  it 'callbacks only fire on change', ->
    input = InputCell 1
    output = ComputeCell input, (x) -> if x < 3 then 111 else 222
    callback = spy.new(->)

    output\watch callback

    input\set_value 2
    assert.spy(callback).was_called 0

    input\set_value 4
    assert.spy(callback).was_called 1
    assert.spy(callback).was_called_with 222

  it 'callbacks can be added and removed', ->
    input = InputCell 11
    output = ComputeCell input, (x) -> x + 1
    callback1 = spy.new(->)
    callback2 = spy.new(->)
    callback3 = spy.new(->)

    output\watch callback1
    output\watch callback2
    input\set_value 31

    output\unwatch callback1
    output\watch callback3
    input\set_value 41

    assert.spy(callback1).was_called 1
    assert.spy(callback1).was_called_with 32
    assert.spy(callback2).was_called 2
    assert.spy(callback2).was_called_with 42
    assert.spy(callback3).was_called 1
    assert.spy(callback3).was_called_with 42

  it "removing a callback multiple times doesn't interfere with other callbacks", ->
    input = InputCell 1
    output = ComputeCell input, (x) -> x + 1
    callback1 = spy.new(->)
    callback2 = spy.new(->)

    output\watch callback1
    output\watch callback2
    for i = 1, 10
      output\unwatch callback1
    input\set_value 2

    assert.spy(callback1).was_called 0
    assert.spy(callback2).was_called 1
    assert.spy(callback2).was_called_with 3

  it 'callbacks only called once even if multiple inputs change', ->
    input = InputCell 1
    plus_one = ComputeCell input, (x) -> x + 1
    minus_one1 = ComputeCell input, (x) -> x - 1
    minus_one2 = ComputeCell minus_one1, (x) -> x - 1
    output = ComputeCell plus_one, minus_one2, (x, y) -> x * y
    callback = spy.new(->)

    output\watch callback
    input\set_value 4
    assert.spy(callback).was_called 1
    assert.spy(callback).was_called_with 10

  it "callbacks not called if inputs change but output doesn't", ->
    input = InputCell 1
    plus_one = ComputeCell input, (x) -> x + 1
    minus_one = ComputeCell input, (x) -> x - 1
    always_two = ComputeCell plus_one, minus_one, (x, y) -> x - y
    callback = spy.new(->)

    always_two\watch callback
    input\set_value i for i = 1, 10
    assert.spy(callback).was_called 0
