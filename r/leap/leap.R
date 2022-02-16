# the divisor evenly divides the number
"%div%" <- function(number, divisor) {
  number %% divisor == 0
}

leap <- function(year) {
  year %div% 400 || (! year %div% 100 && year %div% 4)
}
