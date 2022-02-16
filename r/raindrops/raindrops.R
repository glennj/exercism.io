# the divisor evenly divides the number
"%div%" <- function(number, divisor) {
  number %% divisor == 0
}

raindrops <- function(number) {
  result <- ""

  if (number %div% 3)
    result <- paste0(result, "Pling")

  if (number %div% 5)
    result <- paste0(result, "Plang")

  if (number %div% 7)
    result <- paste0(result, "Plong")

  if (result == "")
    result <- as.character(number)

  result
}
