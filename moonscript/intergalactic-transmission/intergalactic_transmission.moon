parity = (n) ->
  count = 0
  count += (n >> i) & 1 for i = 0, 7
  count & 1

addParity = (n) -> n | parity(n)

checkParity = (n) -> (n & 1) == parity(n >> 1)

fmt = (byte) -> string.format '0x%02x', byte

addByte = (list, byte) -> table.insert list, fmt byte

{
  transmitSequence: (sequence) ->
    return {} if #sequence == 0

    bytes = [tonumber b for b in *sequence]
    message = {}

    addToMessage = (byte) ->
      table.insert message, fmt addParity byte

    current = 0
    leftovers = 0
    leftoverSize = 0

    while #bytes > 0
      leftoverSize += 1
      current = leftovers << 8 | table.remove bytes, 1
      addByte message, addParity((current >> leftoverSize) << 1)

      leftovers = current & ((1 << leftoverSize) - 1)
      if leftoverSize == 7
        addByte message, addParity(leftovers << 1)
        leftovers = 0
        leftoverSize = 0

    if leftoverSize > 0
      addByte message, addParity(leftovers << 8 - leftoverSize)

    message

    
  decodeMessage: (message) ->
    return {} if #message == 0
    
    bytes = [tonumber b for b in *message]
    sequence = {}

    getByte = ->
      byte = table.remove bytes, 1
      assert checkParity(byte), 'wrong parity'
      byte >> 1

    current = getByte!
    shiftSize = 6

    while #bytes > 0
      current = (current << 7) | getByte!
      if shiftSize >= 0
        addByte sequence, current >> shiftSize
        current = current & ((1 << shiftSize) - 1)
      else
        shiftSize = 7
      shiftSize -= 1

    sequence
}
