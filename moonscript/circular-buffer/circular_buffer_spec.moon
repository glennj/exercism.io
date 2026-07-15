CircularBuffer = require 'circular_buffer'

describe 'circular-buffer:', ->
  it 'reading empty buffer should fail', ->
    buffer = CircularBuffer 1
    value, ok = buffer\read!
    assert.is_false ok

  it 'can read an item just written', ->
    buffer = CircularBuffer 1
    ok = buffer\write 1
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 1, value

  it 'each item may only be read once', ->
    buffer = CircularBuffer 1
    ok = buffer\write 1
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 1, value
    value, ok = buffer\read!
    assert.is_false ok

  it 'items are read in the order they are written', ->
    buffer = CircularBuffer 2
    ok = buffer\write 1
    assert.is_true ok
    ok = buffer\write 2
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 1, value
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 2, value

  it "full buffer can't be written to", ->
    buffer = CircularBuffer 1
    ok = buffer\write 1
    assert.is_true ok
    ok = buffer\write 2
    assert.is_false ok

  it 'a read frees up capacity for another write', ->
    buffer = CircularBuffer 1
    ok = buffer\write 1
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 1, value
    ok = buffer\write 2
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 2, value

  it 'read position is maintained even across multiple writes', ->
    buffer = CircularBuffer 3
    ok = buffer\write 1
    assert.is_true ok
    ok = buffer\write 2
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 1, value
    ok = buffer\write 3
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 2, value
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 3, value

  it "items cleared out of buffer can't be read", ->
    buffer = CircularBuffer 1
    ok = buffer\write 1
    assert.is_true ok
    buffer\clear!
    value, ok = buffer\read!
    assert.is_false ok

  it 'clear frees up capacity for another write', ->
    buffer = CircularBuffer 1
    ok = buffer\write 1
    assert.is_true ok
    buffer\clear!
    ok = buffer\write 2
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 2, value

  it 'clear does nothing on empty buffer', ->
    buffer = CircularBuffer 1
    buffer\clear!
    ok = buffer\write 1
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 1, value

  it 'overwrite acts like write on non-full buffer', ->
    buffer = CircularBuffer 2
    ok = buffer\write 1
    assert.is_true ok
    buffer\write 2, overwrite: true
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 1, value
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 2, value

  it 'overwrite replaces the oldest item on full buffer', ->
    buffer = CircularBuffer 2
    ok = buffer\write 1
    assert.is_true ok
    ok = buffer\write 2
    assert.is_true ok
    buffer\write 3, overwrite: true
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 2, value
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 3, value

  it 'overwrite replaces the oldest item remaining in buffer following a read', ->
    buffer = CircularBuffer 3
    ok = buffer\write 1
    assert.is_true ok
    ok = buffer\write 2
    assert.is_true ok
    ok = buffer\write 3
    assert.is_true ok
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 1, value
    ok = buffer\write 4
    assert.is_true ok
    buffer\write 5, overwrite: true
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 3, value
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 4, value
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 5, value

  it 'initial clear does not affect wrapping around', ->
    buffer = CircularBuffer 2
    buffer\clear!
    ok = buffer\write 1
    assert.is_true ok
    ok = buffer\write 2
    assert.is_true ok
    buffer\write 3, overwrite: true
    buffer\write 4, overwrite: true
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 3, value
    value, ok = buffer\read!
    assert.is_true ok
    assert.are.equal 4, value
    value, ok = buffer\read!
    assert.is_false ok

