library(magrittr)

is_isogram <- function(word) {
  chars <- word %>% gsub("[^[:alpha:]]", "", .) %>%
                    tolower %>%
                    strsplit("") %>%
                    unlist
  length(chars) == length(unique(chars))
}
