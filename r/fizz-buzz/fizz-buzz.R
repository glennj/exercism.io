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
