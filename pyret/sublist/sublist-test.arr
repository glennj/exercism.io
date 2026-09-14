use context starter2024

include file("sublist.arr")

check "empty lists":
  sublist([list: ], [list: ]) is "equal"
end

check "empty list within non empty list":
  sublist([list: ], [list: 1, 2, 3]) is "sublist"
end

check "non empty list contains empty list":
  sublist([list: 1, 2, 3], [list: ]) is "superlist"
end

check "list equals itself":
  sublist([list: 1, 2, 3], [list: 1, 2, 3]) is "equal"
end

check "different lists":
  sublist([list: 1, 2, 3], [list: 2, 3, 4]) is "unequal"
end

check "false start":
  sublist([list: 1, 2, 5], [list: 0, 1, 2, 3, 1, 2, 5, 6]) is "sublist"
end

check "consecutive":
  sublist([list: 1, 1, 2], [list: 0, 1, 1, 1, 2, 1, 2]) is "sublist"
end

check "sublist at start":
  sublist([list: 0, 1, 2], [list: 0, 1, 2, 3, 4, 5]) is "sublist"
end

check "sublist in middle":
  sublist([list: 2, 3, 4], [list: 0, 1, 2, 3, 4, 5]) is "sublist"
end

check "sublist at end":
  sublist([list: 3, 4, 5], [list: 0, 1, 2, 3, 4, 5]) is "sublist"
end

check "at start of superlist":
  sublist([list: 0, 1, 2, 3, 4, 5], [list: 0, 1, 2]) is "superlist"
end

check "in middle of superlist":
  sublist([list: 0, 1, 2, 3, 4, 5], [list: 2, 3]) is "superlist"
end

check "at end of superlist":
  sublist([list: 0, 1, 2, 3, 4, 5], [list: 3, 4, 5]) is "superlist"
end

check "first list missing element from second list":
  sublist([list: 1, 3], [list: 1, 2, 3]) is "unequal"
end

check "second list missing element from first list":
  sublist([list: 1, 2, 3], [list: 1, 3]) is "unequal"
end

check "first list missing additional digits from second list":
  sublist([list: 1, 2], [list: 1, 22]) is "unequal"
end

check "order matters to a list":
  sublist([list: 1, 2, 3], [list: 3, 2, 1]) is "unequal"
end

check "same digits but different numbers":
  sublist([list: 1, 0, 1], [list: 10, 1]) is "unequal"
end
