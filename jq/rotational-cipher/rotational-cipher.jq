def cryptMap($shift):
  ("abcdefghijklmnopqrstuvwxyz" / "") as $alphabet
  | ($alphabet[$shift:] + $alphabet[0:$shift]) as $rotated
  | reduce range(26) as $i ({};
      . + {($alphabet[$i]): $rotated[$i]}
        + {($alphabet[$i] | ascii_upcase): ($rotated[$i] | ascii_upcase)}
    )
;

cryptMap(.shiftKey) as $map
| .text / ""
| map($map[.] // .)
| join("")
