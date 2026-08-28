numbers =
  [' _ ' .. '| |' .. '|_|' .. '   ']: '0'
  ['   ' .. '  |' .. '  |' .. '   ']: '1'
  [' _ ' .. ' _|' .. '|_ ' .. '   ']: '2'
  [' _ ' .. ' _|' .. ' _|' .. '   ']: '3'
  ['   ' .. '|_|' .. '  |' .. '   ']: '4'
  [' _ ' .. '|_ ' .. ' _|' .. '   ']: '5'
  [' _ ' .. '|_ ' .. '|_|' .. '   ']: '6'
  [' _ ' .. '  |' .. '  |' .. '   ']: '7'
  [' _ ' .. '|_|' .. '|_|' .. '   ']: '8'
  [' _ ' .. '|_|' .. ' _|' .. '   ']: '9'


convert_row = (lines) ->
  digits = ""
  for i = 1, #lines[1], 3
    key = table.concat [line\sub(i, i + 2) for line in *lines]
    digits ..= numbers[key] or '?'
  digits


convert = (input) ->
  assert #input % 4 == 0, 'Number of input lines is not a multiple of four'
  assert #input[1] % 3 == 0, 'Number of input columns is not a multiple of three'

  rows = [convert_row {table.unpack input, i, i + 3} for i = 1, #input, 4]
  table.concat rows, ','


{ :convert }
