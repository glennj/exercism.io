let isIsogram = s => {
  let rec checker = (chars, seen) =>
    switch chars {
    | list{} => true
    | list{c, ...rest} =>
      switch (c, Array.includes(seen, c)) {
      | ("-", _)
      | (" ", _) => checker(rest, seen)
      | (_, false) => checker(rest, Array.concat(seen, [c]))
      | _ => false
      }
    }

  checker(s->String.toLowerCase->String.split("")->List.fromArray, [])
}
