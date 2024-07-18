def pascal:
  def next_row($r):
    reduce range(1; ($r | length) + 1) as $i ([1]; .[$i] = $r[$i - 1] + $r[$i])
  ;

  if .n == (.rows | length)
    then .rows
    else .rows += [ next_row(.rows[-1]) ] | pascal
  end
;

if .count < 1 
  then [] 
  else {n: .count, rows: [[1]]} | pascal
end