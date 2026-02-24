word_key = (word) ->
  letters = [c for c in word\lower!\gmatch('.')]
  table.sort letters
  table.concat letters


find_anagrams = (subject, candidates) ->
  lc = subject\lower!
  key = word_key subject

  [c for c in *candidates when lc != c\lower! and key == word_key c]

return find_anagrams
