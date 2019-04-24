collatz <- function(num) {
  if (num < 1) {
    stop("Invalid number.")
  }
  steps <- 0
  while (num > 1) {
    steps <- steps + 1
    num   <- if (num %% 2 == 0) {
               num / 2
             } else {
               3 * num + 1
             }
  }
  steps
}

collatz_step_counter <- Vectorize(collatz)


### First attempt, manually handle incoming and outgoing vectors:
#
# collatz_step_counter <- function(num) {
#  if (any(num < 1)) {
#    stop("Invalid num")
#  }
#  steps <- rep(0, length(num))
#  repeat {
#    if (all(num == 1)) {
#      break
#    }
#    for (i in 1:length(num)) {
#      if (num[i] > 1) {
#        steps[i] <- steps[i] + 1
#        num[i] <- if (num[i] %% 2 == 0) {
#                    num[i] / 2
#                  } else {
#                    3 * num[i] + 1
#                  }
#      }
#    }
#  }
#  steps
#}
