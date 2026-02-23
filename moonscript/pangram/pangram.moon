alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

is_pangram = (sentence) ->
  seen = {}
  seen[a] = false for a in alphabet\gmatch('.')
  seen[a] = true  for a in sentence\upper!\gmatch("[#{alphabet}]")

  for _, present in pairs seen
    return false if not present
  true
  
return is_pangram
