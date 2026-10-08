// remove sinqle quotes from start and end of the word
let trimQuotes = w => w->String.replaceRegExp(/^'+|'+$/g, "")

// there is no string isEmpty function
let isNotEmpty = w => String.length(w) > 0

// increment the value at key k
let dictIncr = (d: dict<int>, k: string): dict<int> => {
  let v = d->Dict.get(k)->Option.getOr(0)
  d->Dict.set(k, v + 1) // `set` mutates the dict and returns unit
  d
}

let countWords = input =>
  switch input->String.toLowerCase->String.match(/[a-z0-9']+/g) {
  | None => dict{}
  | Some(matches) =>
    matches
    ->Array.keepSome
    ->Array.map(trimQuotes)
    ->Array.filter(isNotEmpty)
    ->Array.reduce(dict{}, (counts, word) => counts->dictIncr(word))
  }
