fizz_buzz <- function(input) {
  sequence <- as.character(1:input)

  replace <- function(divisor, word) {
    if (input >= divisor) {
      sequence[seq(divisor, input, by=divisor)] = word
      # store the updated values in the outer scope
      sequence <<- sequence
    }
  }

  replace(3, "Fizz")
  replace(5, "Buzz") 
  replace(15, "Fizz Buzz") 
  
  return(sequence)
}

################################################################################
# community solutions:
#
# https://exercism.io/tracks/r/exercises/fizz-buzz/solutions/2df16ec88f854dee9bd8b2c9c3368f0e
#
#        per_fizz_buzz <- function(input) {
#          fizz <- !(input %% 3)
#          buzz <- !(input %% 5)
#        
#          responses <- c("Fizz", "Buzz", as.character(input))
#          bools <- c(fizz, buzz, !(fizz || buzz))
#        
#          paste(responses[bools], collapse = " ")
#        }
#        
#        fizz_buzz <- function(input) {
#          sapply(1:input, per_fizz_buzz)
#        }
#
#
# https://exercism.io/tracks/r/exercises/fizz-buzz/solutions/8607335f8eb14e84b477617fd19637fb
#
#        fizz_buzz <- function (input) {
#            sapply(seq(from = 1, length = input), function (n) {
#                if (n %% 15 == 0) "Fizz Buzz" else
#                if (n %%  5 == 0) "Buzz"      else
#                if (n %%  3 == 0) "Fizz"      else
#                as.character(n)
#            })
#        }
#
# https://exercism.io/tracks/r/exercises/fizz-buzz/solutions/dc662f83ca9b4561aaebba18a12274f0
#
#        fizz_buzz <- function(input) {
#          res = 1:input
#          res[which(1:input %% 3 == 0)] = "Fizz"
#          res[which(1:input %% 5 == 0)] = "Buzz"
#          res[which(1:input %% 15 == 0)] = "Fizz Buzz"
#          res
#        }
