use context starter2024

provide: sublist end

fun sublist(list-one, list-two):
  l1 = list-one.length()
  l2 = list-two.length()
  ask:
    | (l1 == l2) and contains(list-one, list-two) then: "equal"
    | (l1  > l2) and contains(list-one, list-two) then: "superlist"
    | (l1 <  l2) and contains(list-two, list-one) then: "sublist"
    | otherwise: "unequal"
  end
end

fun contains(haystack, needle):
  nl = needle.length()
  hl = haystack.length()
  rec contains-iter = lam(idx):
    ask:
      | (idx + nl) > hl then: false
      | equal-now(slice(haystack, idx, nl), needle) then: true
      | otherwise: contains-iter(idx + 1)
    end
  end
  contains-iter(0)
end

fun slice(lst, idx, len):
  lst.drop(idx).take(len)
end
