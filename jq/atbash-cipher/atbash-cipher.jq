def makeMap:
  # letters: map "a" => "z", ...
  ( "abcdefghijklmnopqrstuvwxyz" / "" | [., reverse] | transpose )
  +
  # digits: map "0" => "0", ...
  ( [range(10)] | map(tostring | [.,.]) )
  | map({key: first, value: last}) 
  | from_entries
;

def groups($n): [scan("[[:alnum:]]{1,\($n)}")] | join(" ");

def encipher:
  makeMap as $mapping
  | ascii_downcase | split("") | map($mapping[.] // "") | join("")
;

.property as $property
| .input.phrase
| encipher
| if $property == "encode"
    then groups(5)
    else .
  end
