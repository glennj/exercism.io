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

# array repeat: 
#   ["x"] | repeat(4) # => ["x","x","x","x"]
#   [1,2] | repeat(3) # => [1,2,1,2,1,2]
def repeat(n):
  . as $a
  | reduce range(n) as $i ([]; . + $a)
;
