# Using the Binary numeral system implementation from
# https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)

def intdiv($numerator; $denominator):
  ($numerator / $denominator) | trunc
;

# find b, the largest power of 4 <= radicand
def init_b:
  pow(4; (((. | log) / (4 | log)) | floor))
;

def find_root($root; $num; $b):
  if $b == 0 then
    $root
  elif $num >= $root + $b then
    find_root(intdiv($root; 2) + $b; $num - $root - $b; intdiv($b; 4))
  else
    find_root(intdiv($root; 2); $num; intdiv($b; 4))
  end
;

.radicand | find_root(0; .; init_b)
