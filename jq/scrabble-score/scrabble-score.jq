# this function will output `empty` for unknown letters
def score:
  ["", "AEIOULNRST", "DG", "BCMP", "FHVWY", "K", "", "", "JX", "", "QZ"] as $scores
  | ascii_upcase as $letter
  | range($scores | length)
  | select($scores[.] | contains($letter))
;

.word
| split("")
| map(score)
| add + 0
