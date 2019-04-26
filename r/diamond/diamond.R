library(stringi)

chr <- function(n) rawToChar(as.raw(n))
ord <- function(c) as.integer(charToRaw(c))

A <- ord("A")

diamond <- function(letter) {
  if (letter == "A") return(letter)

  size <- ord(letter) - A + 1

  top_right <- sapply(1:size, function(i) {
    segment <- rep(" ", size)
    segment[i] <- chr(A - 1 + i)
    paste0(segment, collapse = "")
  })

  top_half <- sapply(top_right, function(half) {
    paste0(
      stri_reverse(substr(half, 2, size)),
      half
    )
  })

  bottom_half <- rev(top_half)[2:size]

  paste(c(top_half, bottom_half), collapse = "\n")
}
