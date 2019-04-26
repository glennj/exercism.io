handshake <- function(n) {
  actions <- c("wink", "double blink", "close your eyes", "jump")
  bits <- intToBits(n)
  action_bits <- bits[1:length(actions)]

  if (all(action_bits == 0)) return(c())

  is_reverse <- bits[length(actions) + 1] == 1
  func <- ifelse(is_reverse, rev, identity)

  func(actions[action_bits == 1])
}
