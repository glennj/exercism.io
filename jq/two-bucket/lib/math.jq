# Some math functions

def gcd($a; $b):
  if $b == 0 then $a else gcd($b; $a % $b) end
;

