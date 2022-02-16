# Determine whether the number is valid.
library(magrittr)

is_valid <- function(input) {
  if (grepl("[^[:digit:][:blank:]]", input)) return(FALSE)

  digits <- input %>% gsub("[[:blank:]]", "", .) %>%
                      strsplit("") %>%
                      unlist %>%
                      as.numeric %>%
                      rev

  if (length(digits) < 2) return(FALSE)

  luhn_digit <- function(i) {
    d <- digits[i]
    if (i %% 2 == 0) {
      d <- 2 * d
      if (d > 9) d <- d - 9
    }
    d
  }

  luhn_digits <- sapply(1:length(digits), luhn_digit)
  sum(luhn_digits) %% 10 == 0
}
