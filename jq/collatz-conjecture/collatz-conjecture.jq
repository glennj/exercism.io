include "lib/assert";

def steps:
  def steps($count):
    if . == 1 then $count
    else
      if . % 2 == 0 then . / 2 else . * 3 + 1 end
      | steps($count + 1)
    end
  ;

  assert(. > 0; "Only positive integers are allowed")
  | steps(0);
