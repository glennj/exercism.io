library(magrittr)

is_pangram <- function(input) {
  re <- paste(c("[^", letters, "]"), collapse = "")
  uniq_chars <- input           %>%
                tolower         %>%
                gsub(re, "", .) %>%
                strsplit("")    %>%
                unlist          %>%
                unique
  length(uniq_chars) == length(letters)
}
