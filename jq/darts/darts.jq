def ring:
  hypot(.x; .y)
  | if   . <= 1  then "bullseye"
    elif . <= 5  then "inner"
    elif . <= 10 then "outer"
    else "miss"
    end
;

{bullseye: 10, inner: 5, outer: 1}[ring] // 0