let abbreviate = phrase => {
  let isLetter = char => "A" <= char && char <= "Z"
  let isWordChar = char => isLetter(char) || char === "'"
  let reducer = ((acronym, state), char) => {
    switch (state, isLetter(char), isWordChar(char)) {
    | ("seekingLetter", true, _) => (acronym ++ char, "seekingNonLetter")
    | ("seekingLetter", false, _) => (acronym, state)
    | (_, _, true) => (acronym, state)
    | (_, _, false) => (acronym, "seekingLetter")
    }
  }
  let (acronym, _) =
    phrase
    ->String.toUpperCase
    ->String.split("")
    ->Belt.Array.reduce(("", "seekingLetter"), reducer)

  acronym
}
