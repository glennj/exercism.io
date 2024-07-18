# input: an array of strings
# output: the annotated array of strings
#
# example: ["*..","..."] => ["*1.","11."]
def annotate:

  def increment_neighbours($x; $y):
    reduce (-1,0,1) as $dx (.;
      reduce (-1,0,1) as $dy (.;
        if  0 <= $x + $dx and $x + $dx < (.input | length) and
            0 <= $y + $dy and $y + $dy < (.input[$x] | length)
        then
          .annotated[$x + $dx][$y + $dy] += 1
        end
      )
    )
  ;

  {
    input: map(. / ""),
    annotated: map(explode | map(0))
  }

  | reduce range(.input | length) as $x (.;
      reduce range(.input[$x] | length) as $y (.;
        if .input[$x][$y] == "*" then
          .annotated[$x][$y] = 99
          | increment_neighbours($x; $y)
        end
      )
    )
  | .annotated
;


def stringify:
  map(
    map(if . == 0 then "." elif . >= 99 then "*" else tostring end)
    | join("")
  )
;


split("\n")[0:-1]
| annotate
| stringify
