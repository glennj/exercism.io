class CircularBuffer
  constructor: (@capacity) ->
    @clear()

  clear:  -> @buffer = []

  isFull: -> @buffer.length == @capacity
  isEmpty: -> @buffer.length == 0

  write: (value) ->
    throw new Error 'full buffer' if @isFull()
    @buffer.push(value)

  read: ->
    throw new Error 'empty buffer' if @isEmpty()
    @buffer.shift()

  overwrite: (value) ->
    @read() if @isFull()
    @write(value)

module.exports = CircularBuffer
