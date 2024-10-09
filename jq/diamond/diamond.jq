def letterIndex($letter): "ABCDEFGHIJKLMNOPQRSTUVWXYZ" | index($letter);
def letterAtIndex($i):    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[$i : $i+1];

def halfLine($n; $size):
  "." * $size / ""
  | .[$n] = letterAtIndex($n)
  | join("")
;

(letterIndex(.letter) + 1) as $size

# build the top-right quadrant
| [range($size) | halfLine(.; $size)]

# fill in the top-left quadrant
| map(. / "" | reverse + .[1:] | join(""))

# and then the bottom half
| . + reverse[1:]
| join("\n")
