library(magrittr)

word_count <- function(phrase) {
  # m <- gregexpr("[[:alnum:]]+", phrase)
  # as.list(table(regmatches(tolower(phrase), m)))

  return(phrase
     %>% gregexpr("[[:alnum:]]+", .)     # find the words
     %>% regmatches(tolower(phrase), .)  # extract them
     %>% table                           # count them
     %>% as.list)
}
