parse_phone_number <- function(number_string) {
  # remove all non-digits
  digits <- gsub("[^0-9]", "", number_string)

  # A valid NANP number:
  # - starts optionally with country code 1,
  # - followed by 2 groups of 3 digits, where the first
  #   digit cannot be 0 or 1,
  # - followed by 4 digits,
  # for a total length of 10 or 11 digits.
  # Capture the last 10 digits

  m <- regexec(
    "(?x) ^1? ((?: [2-9][0-9]{2} ){2} [0-9]{4}) $",
    digits,
    perl = TRUE
  )
  if (m[[1]][1] != -1) regmatches(digits, m)[[1]][2]
}
