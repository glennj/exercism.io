.number as $n
| {Pling: 3, Plang: 5, Plong: 7}
| to_entries
| map(select($n % .value == 0))
| if length == 0
    then $n
    else map(.key) | add
  end
