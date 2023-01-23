if (.strand1 | length) != (.strand2 | length)
  then "strands must be of equal length" | halt_error
  else
    [ (.strand1 | explode), (.strand2 | explode) ]
    | transpose
    | map(select(first != last))
    | length
end
