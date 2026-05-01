List = require 'pl.List'
import fold from require 'moon'

railIterator = (n) ->
  coroutine.wrap ->
    while true
      coroutine.yield i for i = 1, n - 1
      coroutine.yield j for j = n, 2, -1

generateIndices = (n, len) ->
  nextRail = railIterator n
  rails = [{} for _ = 1, n]
  for i = 1, len
    rail = nextRail!
    table.insert rails[rail], i
  -- concatenating a list of lists is not exactly straightward in Lua
  fold {List!, table.unpack rails}, (result, list) -> result\extend list

encipher = (n, text, type) ->
  indices = generateIndices n, #text
  chars = [c for c in text\gmatch '.']
  result = {}
  for i = 1, #text
    switch type
      when 'E' then result[i] = chars[indices[i]]
      when 'D' then result[indices[i]] = chars[i]
  table.concat result

{
  encode: (n, plaintext) -> encipher n, plaintext, 'E'
  decode: (n, ciphertext) -> encipher n, ciphertext, 'D'
}
