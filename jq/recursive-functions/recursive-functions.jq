# Add the elements of the input array, and return the sum.
#
# Example:
#   [1, 2, 3] | array_add           # => 6

def array_add:
  if length == 0
  then 0
  else .[0] + (.[1:] | array_add)
  end;

# Reverse the input array, and return the result in a new array.
#
# Example:
#   [1, 2, 3] | array_reverse       # => [3, 2, 1]

def array_reverse:
  if length == 0
  then []
  else (.[1:] | array_reverse) + [.[0]]
  end;

# Run the filter `f` for each element of the input array,
# and return the outputs in a new array.
#
# Example:
#   [1, 2, 3] | array_map(. + 1)    # => [2, 3, 4]

# implemented with a local function to allow tail call optimization
def array_map(f):
  def _map:
    if (.array | length) == 0
    then .result
    else
      .result += [.array[0] | f]
      | .array = .array[1:]
      | _map
    end;

  {result: [], array: .} | _map;
