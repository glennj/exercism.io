library(magrittr)


sum_of_multiples <- function(factors, limit) {
  ### Take 1, iterative
  #  multiples <- c()
  #  # discard any factors that are not smaller than the limit
  #  for (f in Filter(function(f) f < limit, factors)) {
  #    multiples <- append(multiples, seq(f, limit - 1, by = f))
  #  }
  #  sum(unique(multiples))
  ###

  # Take 2, more functional

  # get the multiples of a factor that are less than the limit
  multiples <- function(f) seq(f, limit - 1, by = f)

  # discard any factors that are not smaller than the limit
  Filter(function(f) f < limit, factors) %>%
    sapply(multiples) %>%
    unlist            %>%
    unique            %>%
    sum
}


# community
#
# beautiful use of `range` and vectorized operations to avoid `unique`
# https://exercism.io/tracks/r/exercises/sum-of-multiples/solutions/612b2d201535490f9ee27aabd144b04e
#
#   sum_of_multiples <- function(factors, limit) {
#     range <- 1:(limit-1)
#     
#     isFactor <- function(number) {
#       any(number %% factors == 0)
#     }
#     
#     sum(Filter(isFactor, range))
#   }
#
#
# the above, taked to extremes
# https://exercism.io/tracks/r/exercises/sum-of-multiples/solutions/7ab050ae8800471b8c85bdf029d8cca7
#   
#   sum_of_multiples <- function(factors, limit) {
#       l <- 1:(limit - 1)
#       sum(l[sapply(l, function(x) 0 %in% (x %% factors))])
#   }
