def isLower: inside("abcdefghijklmnopqrstuvwxyz");

# `break`-ing the `reduce` will cause empty output
def isogramTest:
  label $non_isogram
  | reduce ((.phrase | ascii_downcase) | split("") | map(select(isLower)))[] as $letter ({};
      if has($letter)
        then break $non_isogram
        else .[$letter] = true
      end
    )
;

isempty(isogramTest) | not
