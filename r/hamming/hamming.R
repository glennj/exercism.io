hamming <- function(strand1, strand2) {
  if (nchar(strand1) != nchar(strand2)) {
    stop("Unequal lengths.")
  }

  chars <- strsplit(c(strand1, strand2), "")

  ### naive first take
  # distance <- 0
  # i <- 0
  # while (i < nchar(strand1)) {
  #   i <- i + 1
  #   if (chars[[1]][i] != chars[[2]][i]) {
  #     distance <- distance + 1
  #   }
  # }
  # distance

  sum(chars[[1]] != chars[[2]])
}
