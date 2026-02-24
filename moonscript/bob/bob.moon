response = (input) ->
  trimmed = input\gsub("%s*$", "")

  shh  = trimmed == ''
  ask  = trimmed\match("[?]$")
  yell = trimmed\match("%u") and not trimmed\match("%l")

  if     ask and yell then "Calm down, I know what I'm doing!"
  elseif yell         then "Whoa, chill out!"
  elseif ask          then "Sure."
  elseif shh          then "Fine. Be that way!"
  else                     "Whatever."

{ hey: response }
