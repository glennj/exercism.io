-- some constants
SHIFT_AMT = 7
MSB  = tonumber('10000000', 2)
MASK = tonumber('01111111', 2)

decode = (bytes) ->
  assert bytes[#bytes] & MSB == 0, 'incomplete sequence'
  
  values = {}
  n = 0
  for i = 1, #bytes
    byte = bytes[i]
    n = (n << SHIFT_AMT) + (byte & MASK)
    if byte & MSB == 0
      table.insert values, n
      n = 0
  values

encode = (values) ->
  bytes = {}
  for i = #values, 1, -1
    val = values[i]
    msb = 0
    while true
      byte = (val & MASK) | msb
      table.insert bytes, 1, byte
      msb = MSB
      val >>= SHIFT_AMT
      break if val == 0
  bytes

{ :encode, :decode }
