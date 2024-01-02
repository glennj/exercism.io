use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide:
  my-append, 
  my-concatenate,
  my-filter,
  my-length,
  my-map,
  my-foldl,
  my-foldr,
  my-reverse
end

# Everything _can_ be implement with foldl...

fun my-foldl(lst, f, initial):
  rec helper = lam(xs, acc):
    cases(List) xs:
      | empty => acc
      | link(first, rest) => helper(rest, f(acc, first))
    end
  end
  helper(lst, initial)
end

fun my-append(lst1, lst2):
  #|
  cases(List) lst2:
    | empty => lst1
    | link(first, rest) => my-append(lst1 + [list: first], rest)
  end
  |#

  my-foldl(lst2, {(acc, elem): acc + [list: elem]}, lst1)
end

fun my-concatenate(lst):
  #|
  cases(List) lst:
    | empty => [list:]
    | link(first, rest) => my-append(first, my-concatenate(rest))
  end
  |#

  my-foldl(lst, {(concat, elem): my-append(concat, elem)}, [list:])
end

fun my-filter(lst, f):
  #|
  rec helper = lam(xs, filtered):
    cases(List) xs:
      | empty => filtered
      | link(first, rest) => 
          if f(first):
            helper(rest, filtered + [list: first])
          else:
            helper(rest, filtered)
          end
    end
  end
  helper(lst, [list:])
  |#

  my-foldl(
    lst,
    lam(folded, elem):
      folded + (if f(elem): [list: elem] else: [list:] end)
    end,
    [list:])
end

fun my-length(lst):
  #|
  rec helper = lam(xs, len):
    cases(List) xs:
      | empty => len
      | link(_, rest) => helper(rest, len + 1)
    end
  end
  helper(lst, 0)
  |#

  my-foldl(lst, {(len, _): len + 1}, 0)
end

fun my-map(lst, f):
  #|
  rec helper = lam(xs, mapped):
    cases(List) xs:
      | empty => mapped
      | link(first, rest) => helper(rest, mapped + [list: f(first)])
    end
  end
  helper(lst, [list:])
  |#

  my-foldl(lst, {(mapped, elem): mapped + [list: f(elem)]}, [list:])
end

fun my-reverse(lst):
  my-foldl(lst, {(reversed, elem): my-append([list: elem], reversed)}, [list:])
end

fun my-foldr(lst, f, initial):
  my-foldl(my-reverse(lst), {(acc, elem): f(elem, acc)}, initial)
end
