include "lib/assert";

def steps:
  def _next_collatz:
    if . % 2 == 0
      then . / 2
      else . * 3 + 1
    end
  ;

  def _steps:
    . as [$num, $step]
    | if $num == 1
        then $step
        else [($num | _next_collatz), $step + 1] | _steps
      end
  ;

  assert(. > 0; "Only positive integers are allowed")
  | [., 0] | _steps
;
