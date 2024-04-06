include std/regex.e as re
include std/text.e

public function hey(sequence input)
  sequence trimmed = trim(input)

  atom isSilent = length(trimmed) = 0
  atom isAsking = re:has_match(re:new("[?]$"), trimmed)
  atom isYelling = re:has_match(re:new("[[:upper:]]"), trimmed)
                   and not re:has_match(re:new("[[:lower:]]"), trimmed)

  if isSilent then
    return "Fine. Be that way!"
  elsif isAsking and isYelling then
    return "Calm down, I know what I'm doing!"
  elsif isAsking then
    return "Sure."
  elsif isYelling then
    return "Whoa, chill out!"
  else
    return "Whatever."
  end if
end function
