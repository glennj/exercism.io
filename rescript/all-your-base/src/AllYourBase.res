let toDecimal = (digits, base) => digits->Array.reduce(0, (dec, d) => dec * base + d)

let listIsEmpty = lst => lst->List.head->Option.isNone

let rec toOutDigits = (decimal, base, digits) =>
  switch (decimal, digits->listIsEmpty) {
  | (0, false) => digits->List.toArray
  | (0, true) => [0]
  | _ => toOutDigits(decimal / base, base, digits->List.add(decimal % base))
  }

let rebase = (inBase, inDigits, outBase) => {
  if inBase < 2 || outBase < 2 {
    None
  } else if !Array.every(inDigits, d => 0 <= d && d < inBase) {
    None
  } else {
    inDigits
    ->toDecimal(inBase)
    ->toOutDigits(outBase, list{})
    ->Some
  }
}
