# Inspired by Ruby
#   [1, 2, 3, 4] | each_cons(2)
#   => [1, 2]
#      [2, 3]
#      [3, 4]
#
def each_cons(n):
  . as $a
  | foreach range(length - n + 1) as $i (n - 1; . + 1; $a[$i:.])
;
