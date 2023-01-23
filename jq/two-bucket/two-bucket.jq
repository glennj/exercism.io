include "lib/assert";       # apparently includes must be before imports
import "bucket" as Bucket;
import "lib/math" as Math;

def validate:
  assert(.goal <= ([.bucketOne, .bucketTwo] | max); "impossible")
  | Math::gcd(.bucketOne; .bucketTwo) as $gcd
  | assert($gcd == 1 or .goal % $gcd == 0; "impossible")
;

# input is object {b1: aBucket, b2: otherBucket, goal: aNumber}
def solve:
  def _winner(goal; other):
    {
      moves,
      goalBucket: (goal | Bucket::name),
      otherBucket: (other | Bucket::amount)
    }
  ;

  def _solve:
    # two base cases
    if   (.b1 | Bucket::amount) == .goal then _winner(.b1; .b2)
    elif (.b2 | Bucket::amount) == .goal then _winner(.b2; .b1)
    else
      # recursive case
      if   .b1 | Bucket::isEmpty then .b1 |= Bucket::fill
      elif .b2 | Bucket::isFull  then .b2 |= Bucket::empty
      else 
        Bucket::pour(.b1; .b2) as [$b1, $b2]
        | .b1 = $b1
        | .b2 = $b2
      end
      | .moves += 1
      | _solve
    end
  ;

  # first move: fill b1
  .b1 |= Bucket::fill | .moves = 1

  # (potential) second move: fill b2 if its the right size
  | if .b2.size == .goal
      then .b2 |= Bucket::fill | .moves += 1
      else .
    end

  | _solve
;

####

validate
| Bucket::new("one"; .bucketOne) as $one
| Bucket::new("two"; .bucketTwo) as $two
| if .startBucket == "one"
    then {b1: $one, b2: $two, goal}
    else {b1: $two, b2: $one, goal}
  end
| solve
