etl <- function(input) {
  result <- list()
  for (score in names(input)) {
    #n <- as.integer(score)
    #for (letter in input[[score]]) {
    #  result[tolower(letter)] <- n
    #}

    result[tolower(input[[score]])] <- as.integer(score)
  }
  result[order(names(result))]
}
