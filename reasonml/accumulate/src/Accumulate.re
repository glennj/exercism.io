let accumulate = (func, elems) => {
  let rec do_accumulate = (xs, accum) => {
    switch (xs) {
    | [] => List.rev(accum)
    | [x, ...rest] => do_accumulate(rest, [func(x), ...accum])
    }
  }

  do_accumulate(elems, [])
};
