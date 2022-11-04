.heyBob
| sub("\\s+$"; "")      # remove trailing whitespace
| {
    silence: (length == 0),
    yelling: (test("[[:upper:]]") and (test("[[:lower:]]") | not)),
    asking:  endswith("?")
  }
| if .silence then "Fine. Be that way!"
  elif .yelling and .asking then "Calm down, I know what I'm doing!"
  elif .yelling then "Whoa, chill out!"
  elif .asking then "Sure."
  else "Whatever."
  end
