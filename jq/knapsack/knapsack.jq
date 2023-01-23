# Using algorithm from https://en.wikipedia.org/wiki/Knapsack_problem

import "lib/arrays" as Array;

def matrix2d(num_rows; num_cols; init_value):
  (
    [ [init_value] | Array::repeat(num_cols) ]
    | Array::repeat(num_rows)
  )
;

def maximize_value(i; j):
  if i <= 0 then .value[0][j] = 0
  elif j <= 0 then .value[i][0] = 0
  else
    if .value[i-1][j] == -1 then maximize_value(i-1; j) else . end
    | if .w[i] > j
      then .value[i][j] = .value[i-1][j]
      else
        if .value[i-1][j - .w[i]] == -1 then maximize_value(i-1; j - .w[i]) else .  end
        | .value[i][j] = ([
                            .value[i-1][j],
                            .value[i-1][j - .w[i]] + .v[i]
                          ] | max)
      end
  end
;

.maximumWeight as $W
| (.items | length) as $n
| # Construct a state object.
  # - note that the algorithm indexes arrays starting at 1, 
  #   so we need to pad the zero'th element
  {
    w: ([null] + (.items | map(.weight))),
    v: ([null] + (.items | map(.value))),
    value: matrix2d($n + 1; $W + 1; -1)
  }
| maximize_value($n; $W)
| .value[$n][$W]
