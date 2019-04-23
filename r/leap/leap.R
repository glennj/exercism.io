# the divisor evenly divides the number
"%div%" <- function(number, divisor) {
  number %% divisor == 0
}

leap <- function(year) {
  year %div% 400 || (! year %div% 100 && year %div% 4)
}

# community
#
# https://exercism.io/tracks/r/exercises/leap/solutions/7301a18c561749749c7915c92c8c9d1d
#
#   leap <- function(year) {
#     test <- year %% c(4,100,400) == c(0,0,0)
#     return(test[3] | (test[1] & !test[2]))
#   }
