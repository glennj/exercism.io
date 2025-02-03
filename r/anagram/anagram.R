library(magrittr)

to_key <- . %>% strsplit("") %>%
                unlist %>%
                sort %>%
                paste0(collapse = "")


anagram <- function(subject, candidates) {
  lc_subj <- tolower(subject)
  key <- to_key(lc_subj)

  # closure: determine if a candidate word is an anagram
  # of the subject
  is_anagram <- function(candidate) {
    lc_cand <- tolower(candidate)
    lc_subj != lc_cand && key == to_key(lc_cand)
  }

  anagrams <- Filter(is_anagram, candidates)
  if (length(anagrams) > 0) anagrams else c()
}
