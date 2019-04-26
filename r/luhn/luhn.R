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


# community
#
# usage of logic indices
# https://exercism.io/tracks/r/exercises/luhn/solutions/c2987c0ad8ed4e33aa0a9b60c0a5eb00
#
#   # Determine whether the number is valid.
#   is_valid <- function(input) {
#     
#     # Strip spaces, check length & check for invalid characters
#     input_vector <- strsplit(gsub(pattern = " ", replacement = "", input), "")[[1]]
#     if (length(input_vector) < 2 || any(grepl("[^[:digit:]]", input_vector))) {
#       return (FALSE)
#     }
#     
#     # Convert to numeric
#     num_vector <- as.numeric(input_vector)
#     
#     # Double every second digit starting from the right
#     num_vector <- rev(num_vector)
#     num_vector[seq(2,length(num_vector),2)] = num_vector[seq(2,length(num_vector),2)]*2
#     
#     # Subtract 9 if > 9 (can apply to all since no digit can be greater than 9 before doubling)
#     num_vector <- ifelse(num_vector > 9, num_vector - 9, num_vector)
#     
#     # Check checksum is divisible by 10
#     sum(num_vector) %% 10 == 0
#     
#   }
