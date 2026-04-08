{
  encode: (plaintext) ->
    text = plaintext\gsub('%W', '')\lower!
    return '' if text == ''

    cols = math.ceil math.sqrt #text
    chunks = [text\sub i, i + cols - 1 for i = 1, #text, cols]
    -- pad the last chunk with spaces if necessary
    chunks[#chunks] = string.format "%-#{cols}s", chunks[#chunks]

    transposed = [table.concat [chunk\sub i, i for chunk in *chunks] for i = 1, cols]
    table.concat transposed, ' '
}
