library(magrittr)

nucleotide_count <- function(input) {
  nucleotides <- c("A", "C", "G", "T")

  re <- paste0(c("[^", nucleotides, "]"), collapse = "")
  if (grepl(re, input)) stop("invalid nucleotide")

  # inspired by @jejones3141
  # https://exercism.io/tracks/r/exercises/nucleotide-count/solutions/e7616fe30a4049cc9ace0a225a5d8104
  input %>% strsplit("") %>%
            unlist       %>%
            factor(levels = nucleotides) %>%
            table        %>%
            as.list

  ### Take 1 - add missing nucleotides after processing input
  # count <- as.list(table(unlist(strsplit(input, ""))))
  #
  # missing <- nucleotides[!(nucleotides %in% names(count))]
  # count[missing] <- rep(0, length(missing))
  # count
}
