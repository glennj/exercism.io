trim_end = (str) -> str\gsub '%s+$', ''
pad_end = (str, wid) -> string.format "%-#{wid}s", str

{
  transpose: (input) ->
    max = math.max 0, table.unpack [#line for line in *input]

    transposed = {}
    for col = 1, max
      new_row = [col > #row and ' ' or row\sub(col, col) for row in *input]
      table.insert transposed, table.concat new_row
    
    wid = 1
    for i = #transposed, 1, -1
      transposed[i] = pad_end trim_end(transposed[i]), wid
      wid = #transposed[i]

    transposed
}
