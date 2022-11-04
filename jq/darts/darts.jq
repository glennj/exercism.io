def score:
  if   . <= 1  then 10
  elif . <= 5  then 5
  elif . <= 10 then 1
  else 0
  end
;

hypot(.x; .y) | score