provide:
  index-of,
  with-index
end

fun index-of(lst, a):
  doc: ```
    Find the index of element a in list lst.
    Returns -1 if a is not in lst.
  ```
  rec finder = lam(i):
    ask:
      | i == lst.length() then: -1
      | a == lst.get(i) then: i
      | otherwise: finder(i + 1)
    end
  end
  finder(0)
where:
  index-of([list: 1, 2, 3], 3) is 2
  index-of([list: 1, 2, 3], 4) is -1
end

fun with-index(lst):
  doc: ```
    Given a list, return a new list of tuples {idx; elem}.
  ```
  map_n(lam(idx, elem): {idx; elem} end, 0, lst)
where:
  with-index([list:]) is [list:]
  with-index([list: 11, 22, 33]) is [list: {0; 11}, {1; 22}, {2; 33}]
end
