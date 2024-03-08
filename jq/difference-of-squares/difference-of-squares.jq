def seq: [range(1; . + 1)];
def square: . * .;

def squareOfSum:  seq | add | square;
def sumOfSquares: seq | map(square) | add;

.property as $prop
| .input.number
| if   $prop == "squareOfSum"         then squareOfSum
  elif $prop == "sumOfSquares"        then sumOfSquares
  elif $prop == "differenceOfSquares" then (squareOfSum - sumOfSquares)
  else "Unknown property \($prop)" | halt_error
  end
