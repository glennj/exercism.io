difference_of_squares <- function(n) {
  if (!is.numeric(n) || n < 0) stop("bad input")
  square_of_sums <- sum(0:n) ^ 2
  sum_of_squares <- sum((0:n) ^ 2)
  square_of_sums - sum_of_squares
}
