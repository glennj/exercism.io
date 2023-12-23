use context essentials2020

provide: flatten end

fun flatten(lst):
  lst.foldl(
    lam(elem, flattened):
      ask:
        | is-nothing(elem) or is-empty(elem) then: flattened
        | is-link(elem) then: flattened.append(flatten(elem))
        | otherwise: flattened.append([list: elem])
      end
    end,
    [list:])
end
