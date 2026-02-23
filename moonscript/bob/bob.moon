response = (input) ->
  trimmed = input\gsub("%s*$", "")

  is_silence  = trimmed == ''
  is_question = trimmed\match("[?]$")
  is_yelling  = trimmed\match("%u") and not trimmed\match("%l")

  return "Calm down, I know what I'm doing!" if is_question and is_yelling
  return "Sure."                             if is_question
  return "Whoa, chill out!"                  if is_yelling
  return "Fine. Be that way!"                if is_silence
  return "Whatever."

{ hey: response }
