library(magrittr)

is_isogram <- function(word) {
  chars <- word %>% gsub("[^[:alpha:]]", "", .) %>%
                    tolower %>%
                    strsplit("") %>%
                    unlist
  length(chars) == length(unique(chars))
}


# community
#
# using `table()`
# https://exercism.io/tracks/r/exercises/isogram/solutions/cf1228baecbc4c2e93163ea7958ec08a
#
#   library(stringr)
#   
#   is_isogram <- function(word) {
#     word %>%
#       str_to_lower() %>% 
#       str_extract_all("[:alpha:]") %>% 
#       table() %>% 
#       all(. == 1)
#   }
#
