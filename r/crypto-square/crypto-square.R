library(stringr)

normalized_plaintext <- function(input) {
  gsub("[^[:alnum:]]", "", tolower(input))
}

plaintext_segments <- function(input) {
  if (input == "") return("")
  normalized <- normalized_plaintext(input)
  len <- ceiling(sqrt(nchar(normalized)))
  m <- gregexpr(sprintf(".{1,%d}", len), normalized, perl = TRUE)
  unlist(regmatches(normalized, m))
}

encoded <- function(input) {
  if (input == "") return("")
  segments <- plaintext_segments(input)
  transposed <- sapply(
    1:nchar(segments[1]),
    function(i) {
      paste0(
        sapply(segments, function(s) substr(s, i, i)),
        collapse = ""
      )
    }
  )
  paste0(transposed, collapse = "")
}

ciphertext <- function(input) {
  if (input == "") return("")
  encoded <- encoded(input)

  num <- ceiling(sqrt(nchar(encoded)))
  leftovers <- nchar(encoded) %% num
  lengths <- c()
  for (i in 1:num) {
    lengths[i] <- nchar(encoded) %/% num +
                  ifelse(i <= leftovers, 1, 0)
  }

  segments <- c()
  start <- 1
  for (i in 1:num) {
    end <- start + lengths[i] - 1
    segments <- append(segments, substr(encoded, start, end))
    start <- end + 1
  }

  paste0(
    str_pad(segments, nchar(segments[1]), "right"),
    collapse = " "
  )
}
