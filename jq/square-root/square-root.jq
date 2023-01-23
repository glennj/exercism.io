# Using the Binary numeral system implementation from
# https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)

include "lib/math";

def isqrt:
  def _isqrt:
    . as [$n, $c, $d]
    | if $d == 0
        then $c
        else
          if $n >= $c + $d
            then [$n - ($c + $d), bshr($c; 1) + $d, bshr($d; 2)] | _isqrt
            else [$n, bshr($c; 1), bshr($d; 2)] | _isqrt
          end
      end
  ;

  # d is the largest power of 4 less than or equal to n
  pow(4; logb(.; 4) | floor) as $d
  | [., 0, $d] | _isqrt
;

.radicand | isqrt
