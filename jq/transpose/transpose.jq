# I'm using `transpose` below. It fills "missing" elements from shorter arrays with null
#
#       $ echo '[[1,2],["a","b","c"]]' | jq -c 'transpose'
#       [[1,"a"],[2,"b"],[null,"c"]]

# input: an array
# output: the array with all trailing nulls removed
def trim_trailing_nulls:
  if length == 0 or last != null
    then .
    else .[:-1] | trim_trailing_nulls
  end
;

# input: an array
# output: the array with any null elements replaced by 1-space strings
def nulls_to_spaces: map(. // " ");

.lines
| map(split(""))
| transpose
| map(trim_trailing_nulls | nulls_to_spaces | join(""))
