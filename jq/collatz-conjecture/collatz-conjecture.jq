include "lib/assert";

def steps:
  def _next_collatz:
    if . % 2 == 0
      then . / 2
      else . * 3 + 1
    end
  ;

  # `while` outputs the current value at each iteration.
  # Capture the outputs and count them for the number of steps.

  assert(. > 0; "Only positive integers are allowed")
  | [while (. != 1; _next_collatz)] | length
;
