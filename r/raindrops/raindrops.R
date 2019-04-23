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

# community
#
# amazingly thorough:
# https://exercism.io/tracks/r/exercises/raindrops/solutions/4e5858d9e31b4b0bb374cec512ad265f
#
#
# with vectors
# https://exercism.io/tracks/r/exercises/raindrops/solutions/57060f71368c409a9c77530ec4f80a18
# 
#   raindrops <- function(number, divisors = list(Pling = 3, Plang = 5, Plong = 7)) {
#     noises <- names(divisors)[number %% unlist(divisors) == 0]
#     if (length(noises) > 0) {
#       paste(noises, collapse="")
#     } else {
#       as.character(number)
#     }
#   }
#
#
# similar
# https://exercism.io/tracks/r/exercises/raindrops/solutions/cb33cb138d4848bd860469aad324649c
#
#   raindrops <- function(number) {
#     factors <- c(3, 5, 7)
#     sounds <- c("Pling", "Plang", "Plong")
#     
#     factors_present <- number %% factors == 0
#     
#     if(any(factors_present))
#       return(paste0(sounds[factors_present], collapse = ""))
#     else
#       return(as.character(number))
#   }
