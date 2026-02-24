-- list comprehension madness

split = (str, delim) ->
  [field for field in str\gmatch "[^#{delim}]+"]


matrix = (str) ->
  lines = split str, "\n"
  mtx = [split line, " " for line in *lines]
  [ [tonumber cell for cell in *row] for row in *mtx]


{
  row: (str, idx) ->
    matrix(str)[idx]

  column: (str, idx) ->
    [row[idx] for row in *matrix(str)]
}
