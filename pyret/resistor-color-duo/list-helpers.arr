provide:
  index-of
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
