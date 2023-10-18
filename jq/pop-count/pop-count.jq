# jq doesn't have bitwise operations built-in, but
# division by 2 is equivalent to right-shift by 1 and
# (n % 2) is equivalent to (n & 1).

def popCount:
  . as [$num, $count]
  | if $num == 0 then
      $count
    else
      [($num / 2 | floor), $count + $num % 2] | popCount
    end
;

[.number, 0] | popCount
