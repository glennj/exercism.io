bob <- function(input) {
  input <- trimws(input)
  if (input == "") return("Fine. Be that way!")

  asking   <- endsWith(input, "?")
  shouting <- grepl("[[:alpha:]]", input) &&
            ! grepl("[[:lower:]]", input)

  if (shouting && asking) "Calm down, I know what I'm doing!"
  else if (shouting)      "Whoa, chill out!"
  else if (asking)        "Sure."
  else                    "Whatever."
}
