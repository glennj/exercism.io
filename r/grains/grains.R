square <- function(n) {
  if (!is.numeric(n) || n < 1 || n > 64) {
    stop("Invalid input.")
  }
  2 ^ (n - 1)
}

total <- function() {
  sum(sapply(1:64, square))
  # or, directly return 2^64 - 1
}
