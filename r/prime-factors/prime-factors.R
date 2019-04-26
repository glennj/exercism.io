prime_factors <- function(n) {
  factors <- c()
  f <- 2
  while (f ^ 2 <= n) {
    if (n %% f == 0) {
      factors <- append(factors, f)
      n <- n / f
    } else {
      f <- f + 1
    }
  }
  if (n > 1) factors <- append(factors, n)
  factors
}
