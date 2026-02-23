{
  is_isogram: (phrase) ->
    seen = {}
    for letter in phrase\lower!\gmatch("%a")
      return false if seen[letter]
      seen[letter] = true
    true
}
