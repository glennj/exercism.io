# Take 2

to_rna <- function(dna) {
  nucleotide_complements <- list(
    "G" = "C",
    "C" = "G",
    "T" = "A",
    "A" = "U"
  )
  nucleotides <- unlist(strsplit(dna, ""))
  if (!all(nucleotides %in% names(nucleotide_complements))) {
    stop("Invalid nucleotide.")
  }
  paste0(nucleotide_complements[nucleotides], collapse = "")
}


### Take 1
# library(magrittr)
# 
# nucleotide_complement <- function(nucleotide) {
#   complement <- switch(nucleotide,
#                         "G" = "C",
#                         "C" = "G",
#                         "T" = "A",
#                         "A" = "U")
#   if (is.null(complement)) stop("Invalid nucleotide")
#   complement
# }
# 
# to_rna <- . %>% strsplit("") %>%
#                 unlist %>%
#                 sapply(nucleotide_complement) %>%
#                 paste0(collapse = "")

### non-magrittr function
# to_rna <- function(dna) {
#   paste0(sapply(unlist(strsplit(dna, "")), nucleotide_complement), collapse = "")
# }
