library(magrittr)


acronym <- function(input) {
  # A "word" pattern: a letter followed by some letters and
  # hyphens. We're counting on regex greediness to find all
  # the full words.
  re <- "[[:alpha:]][[:alpha:]']*"

  return(input
    %>% gregexpr(re, .)         # locate the words
    %>% regmatches(input, .)    # extract them
    %>% unlist                  #
    %>% substr(1, 1)            # first letter of each
    %>% paste(collapse = "")    # join, and
    %>% toupper                 # return the uppercase
  )
}

# or piecemeal without magrittr
acronym_2 <- function(input) {
  re <- "[[:alpha:]][[:alpha:]']*"
  m <- gregexpr(re, input)
  words <- regmatches(input, m)
  inits <- substr(unlist(words), 1, 1)
  toupper(paste(inits, collapse = ""))
}


# with a series of `gsub`s
# too ugly
acronym_3 <- . %>%
  gsub("(?<=[^[:alpha:])([[:lower:]])", "\\U\\1", ., perl = T) %>%
  gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", .) %>%
  gsub("-", " ", .) %>%
  gsub("([[:blank:]])[^[:alpha:]]", "\\1", .) %>%
  abbreviate(minlength = 1) %>%
  unname %>%
  toupper
