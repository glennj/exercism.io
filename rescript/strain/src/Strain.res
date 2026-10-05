// Array.filter is the cheating way to do it

let keep = (ary, fn) => {
  let rec filter = (lst, acc) =>
    switch lst {
    | list{} => acc
    | list{elem, ...rest} =>
        filter(rest, if fn(elem) {acc->List.add(elem)} else {acc})
    }

  ary
  ->List.fromArray
  ->filter(list{})
  ->List.reverse
  ->List.toArray
}

let discard = (ary, fn) => keep(ary, x => !fn(x))
