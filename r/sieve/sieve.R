sieve <- function(limit) {
  if (limit <  2) return(c())
  if (limit == 2) return(c(2))

  nums <- c(NA, 2:limit)

  for (i in 2:sqrt(limit)) {
    if (is.na(nums[i])) next  # seen it already

    # If n is 2, we want to remove every even multiple.
    # If n > 2, then we want to remove every *other*
    # multiple (the even ones are already removed).
    step <- ifelse(i == 2, 2, 2 * i)

    nums[seq(from = i ^ 2, to = limit, by = step)] <- NA
  }

  nums[!is.na(nums)]
}
