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
