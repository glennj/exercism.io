def luhn_sum:
  [0, 2, 4, 6, 8, 1, 3, 5, 7, 9] as $doubled
  | reduce (. / "" | reverse | map(tonumber)[]) as $digit ({double: false, sum: 0};
      .sum += if .double then $doubled[$digit] else $digit end
      | .double |= not
    )
  | .sum;

# spaces are OK
gsub("\\s"; "")
| if test("\\D") then false
  elif . == "0" then false
  else luhn_sum % 10 == 0
  end
