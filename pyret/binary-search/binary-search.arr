use context starter2024

provide: binary-search end

fun binary-search(lst, elt):
  rec searcher = lam(i, j):
    if i > j:
      raise("value not in list")
    else:
      mid = num-floor((i + j) / 2)
      ask:
        | elt < lst.get(mid) then: searcher(i, mid - 1)
        | elt > lst.get(mid) then: searcher(mid + 1, j)
        | otherwise: mid
      end
    end
  end
  searcher(0, lst.length() - 1)
end
