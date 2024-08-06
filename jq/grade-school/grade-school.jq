# input: array of [name, grade] pairs
# output: array of arrays of names, indexed by grade

def to_school:
  reduce .[] as $student ({seen: [], school:[[]]};
    $student as [$name, $grade]
    | .seen as $all
    | if $name | IN($all[]) | not then
        .seen += [$name]
        | .school[$grade] += [$name]
      end
  )
  | .school
  | map(if . == null then [] else sort end)
;

(.input.students | to_school) as $school
| if   .property == "roster" then $school | add
  elif .property == "grade"  then $school[.input.desiredGrade] // []
  end
