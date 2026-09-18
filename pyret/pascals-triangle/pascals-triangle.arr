use context starter2024

provide: rows end

fun rows(count :: NumNonNegative) -> List<List<NumPositive>>:
  ask:
    | count == 0 then: [list: ]
    | count == 1 then: [list: [list: 1]]
    | otherwise:
        prev = rows(count - 1)
        get-zero = list-get-default(prev.last(), _, 0)
        prev + [list: range(0, count).map(lam(i): get-zero(i - 1) + get-zero(i) end)]
  end
end

fun list-get-default(lst, i, default):
  ask:
    | (i < 0) or (i >= lst.length()) then: default
    | otherwise: lst.get(i)
  end
where:
  list-get-default([list: 11, 22, 33], 0, -1) is 11
  list-get-default([list: 11, 22, 33], 2, -1) is 33
  list-get-default([list: 11, 22, 33], -2, 42) is 42
  list-get-default([list: 11, 22, 33], 3, 42) is 42
end
