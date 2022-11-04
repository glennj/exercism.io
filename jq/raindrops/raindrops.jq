## straightforwardly:
#.number as $n
#| [
#    if .number % 3 == 0 then "Pling" else empty end,
#    if .number % 5 == 0 then "Plang" else empty end,
#    if .number % 7 == 0 then "Plong" else empty end
#  ] 
#| join("")
#| if length == 0 then $n else . end

## with `map-select`
#.number as $n
#| [[3,"Pling"], [5,"Plang"], [7,"Plong"]]
#| map(select($n % first == 0))
#| map(last)
#| join("")
#| if length == 0 then $n else . end

## with `reduce`
.number as $n
| reduce ([3,"Pling"], [5,"Plang"], [7,"Plong"]) as $sound ("";
    if $n % ($sound[0]) == 0 then . + $sound[1] else . end
  )
| if length == 0 then $n else . end
