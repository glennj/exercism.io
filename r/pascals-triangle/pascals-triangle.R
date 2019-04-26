pascals_triangle <- function(n) {
  if (n  < 0) stop("Invalid input")
  if (n == 0) return(list())
  lapply(0:(n - 1), function(m) choose(m, 0:m))
}
