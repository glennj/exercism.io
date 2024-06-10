def score_for(die): map(select(. == die)) | add // 0;

def groups: reduce .[] as $die ([0,0,0,0,0,0]; .[$die] += 1) | to_entries;

def full_house:
  (groups | map(select(.value > 0))) as $count
  | if ($count | length == 2) and ($count[0].value == 2 or $count[0].value == 3)
    then add
    else 0
    end
;

def four_of_a_kind:
  (groups | map(select(.value >= 4))) as $count
  | if ($count | length == 1)
    then $count[0].key * 4
    else 0
    end
;

def straight(s): if sort == s then 30 else 0 end;

def yacht: if (unique | length) == 1 then 50 else 0 end;

.category as $cat
| .dice
| if   $cat == "ones"            then score_for(1)
  elif $cat == "twos"            then score_for(2)
  elif $cat == "threes"          then score_for(3)
  elif $cat == "fours"           then score_for(4)
  elif $cat == "fives"           then score_for(5)
  elif $cat == "sixes"           then score_for(6)
  elif $cat == "full house"      then full_house
  elif $cat == "four of a kind"  then four_of_a_kind
  elif $cat == "little straight" then straight([1,2,3,4,5])
  elif $cat == "big straight"    then straight([2,3,4,5,6])
  elif $cat == "yacht"           then yacht
  elif $cat == "choice"          then add
  else 0
  end
