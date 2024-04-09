def assert(cond; err): if (cond | not) then err | halt_error end ;

# input: an object with "inputBase" and "digits" keys
# output: a base-10 integer
def toDecimal:
  assert(.inputBase >= 2; "input base must be >= 2")
  | .inputBase as $iBase
  | reduce .digits[] as $d (0; 
      assert(0 <= $d and $d < $iBase; "all digits must satisfy 0 <= d < input base")
      | . * $iBase + $d
    )
;

def toBase($oBase):
  def to:
    if .n == 0 then
      if (.digits | length) == 0 then .digits += [0] end
      | .digits
    else
      {n: (.n / $oBase | floor), digits: ([.n % $oBase] + .digits)}
      | to
    end
  ;
  assert($oBase >= 2; "output base must be >= 2")
  | {n: ., digits: []}
  | to
;
  
.outputBase as $oBase
| toDecimal
| toBase($oBase)
