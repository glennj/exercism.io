# right-shift $offset bits and test the least bit
def bit_is_on($n; $offset):
  ($n / ($offset | exp2) | floor) % 2 == 1
;

# `to_entries` on an array produces a list of {key, value} objects 
# with the element index as the key:
#    [11,22,33] | to_entries
#    => [{"key":0,"value":11},{"key":1,"value":22},{"key":2,"value":33}]

.number as $n
| ["wink", "double blink", "close your eyes", "jump"]
| length as $len
| to_entries
| map(select(bit_is_on($n; .key)) | .value)
| if bit_is_on($n; $len) then reverse end