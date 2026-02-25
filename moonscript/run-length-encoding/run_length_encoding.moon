{
  encode: (plaintext) ->
    -- find all the unique characters
    uniq = {c, true for c in plaintext\gmatch '.'}

    -- replace runs of two or more of these characters
    encoded = plaintext
    for c, _ in pairs uniq
      encoded = encoded\gsub "#{c}#{c}+", (run) -> "#{#run}#{c}"
    encoded

  decode: (ciphertext) ->
    ciphertext\gsub '(%d+)(%D)', (len, char) -> char\rep len
}
