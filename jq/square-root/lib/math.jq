# Some math functions

def gcd($a; $b):
  if $b == 0 then $a else gcd($b; $a % $b) end
;

def intdiv(numerator; denominator):
  (numerator / denominator) | trunc
;

def logb(num; base):
  ((num | log) / (base | log))
;

# if only jq had bit-shifting operators.

# "bit shift right"
def bshr(n; bits): intdiv(n; pow(2; bits));

# "bit shift left"
def bshl(n; bits): n * pow(2; bits);
