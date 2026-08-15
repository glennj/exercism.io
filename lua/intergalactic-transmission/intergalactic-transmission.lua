-- some forward declarations
local table_slice, table_append_all, table_partition
local to_bits, parity, check_parity, value_of

-- ----------------------------------------
local function transmit_sequence(sequence)
  local msg, byte, seq_bits = {}, nil, to_bits(sequence)
  while #seq_bits > 0 do
    byte, seq_bits = table_partition(seq_bits, 7)
    -- pad with zeroes if needed
    while #byte < 7 do
      byte[#byte + 1] = 0
    end
    byte[#byte + 1] = parity(byte)
    msg[#msg + 1] = value_of(byte)
  end
  return msg
end

local function decode_message(message)
  local seq_bits, byte, msg_bits = {}, nil, to_bits(message)
  while #msg_bits > 0 do
    byte, msg_bits = table_partition(msg_bits, 8)
    assert(check_parity(byte), 'wrong parity')
    table.remove(byte)
    table_append_all(seq_bits, byte)
  end
  local seq = {}
  while #seq_bits >= 8 do
    byte, seq_bits = table_partition(seq_bits, 8)
    seq[#seq + 1] = value_of(byte)
  end
  return seq
end

-- ----------------------------------------
function to_bits(sequence)
  local bits, bs = {}, nil
  for _, n in ipairs(sequence) do
    bs = {}
    while n > 0 do
      table.insert(bs, 1, n & 1)
      n = n >> 1
    end
    while #bs < 8 do
      table.insert(bs, 1, 0)
    end
    table_append_all(bits, bs)
  end
  return bits
end

function parity(bits)
  local sum = 0
  for _, b in ipairs(bits) do
    sum = sum + b
  end
  return sum & 1
end

function check_parity(byte)
  return byte[8] == parity(table_slice(byte, 1, 7))
end

function value_of(bits)
  local val = 0
  for _, b in ipairs(bits) do
    val = (val << 1) | b
  end
  return val
end

-- ----------------------------------------
function table_slice(t, i, j)
  return { table.unpack(t, i, j) }
end

function table_append_all(t, u)
  for _, elem in ipairs(u) do
    t[#t + 1] = elem
  end
end

function table_partition(t, n)
  return table_slice(t, 1, n), table_slice(t, n + 1)
end

-- ----------------------------------------
return { transmit_sequence = transmit_sequence, decode_message = decode_message }
