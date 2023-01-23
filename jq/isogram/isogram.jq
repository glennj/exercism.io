# return a stream of letters from a string
def letters:
  ascii_downcase / ""
  | .[] 
  | select(test("[[:lower:]]"))
;

# `break`-ing the `reduce` will cause empty output
def isogramTest:
  label $non_isogram
  | reduce (.phrase | letters) as $letter ({};
      if has($letter)
        then break $non_isogram
        else .[$letter] = true
      end
    )
;

isempty(isogramTest) | not
