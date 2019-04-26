number_type <- function(n) {
  if (n <= 0) stop("Natural numbers only.")
  if (n == 1) return("deficient")
  aliquot_sum <- 1
  limit <- floor(sqrt(n))
  i <- 2
  while (i <= limit) {
    if (n %% i == 0) {
      aliquot_sum <- aliquot_sum + sum(unique(c(i, n / i)))
    }
    i <- i + 1
  }
  if (aliquot_sum < n)       "deficient"
  else if (aliquot_sum == n) "perfect"
  else                       "abundant"
}
