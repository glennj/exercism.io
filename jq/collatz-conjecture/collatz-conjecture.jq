include "lib/assert";

def steps:
  def _next_collatz:
    if . % 2 == 0
      then . / 2
      else . * 3 + 1
    end
  ;

  def _steps($n):
    if . == 1
      then $n
      else _next_collatz | _steps($n + 1)
    end
  ;

  assert(. > 0; "Only positive integers are allowed") | _steps(0)
;
