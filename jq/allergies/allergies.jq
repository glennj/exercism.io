# right-shift $offset bits and test the least bit
def bit_is_on($n; $offset):
  ($n / ($offset | exp2) | floor) % 2 == 1
;

def listFor($score):
  [ "eggs", "peanuts", "shellfish", "strawberries",
    "tomatoes", "chocolate", "pollen", "cats" ]
  | to_entries
  | map( select(bit_is_on($score; .key)) | .value )
;

listFor(.input.score) as $allergens
| if   .property == "list"       then $allergens
  elif .property == "allergicTo" then .input.item | IN($allergens[])
  end
