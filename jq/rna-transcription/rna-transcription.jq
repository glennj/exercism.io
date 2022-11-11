def toRna:
  explode
  | map({"71": 67, "67": 71, "84": 65, "65": 85}[tostring])
       #  G => C    C => G    T => A    A => U
  | implode
;