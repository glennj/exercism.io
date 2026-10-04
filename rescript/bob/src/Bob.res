let response = input => {
  let trimmed = input->String.trimEnd
  
  let isSilence = trimmed->String.isEmpty
  let isQuestion = trimmed->String.endsWith("?")
  let isYelling = trimmed->String.match(/[A-Z]/)->Option.isSome
               && trimmed->String.match(/[a-z]/)->Option.isNone

  switch (isSilence, isQuestion, isYelling) {
  | (true, _, _) => "Fine. Be that way!"
  | (_, true, true) => "Calm down, I know what I'm doing!"
  | (_, true, _) => "Sure."
  | (_, _, true) => "Whoa, chill out!"
  | (_, _, _) => "Whatever."
  }
}
