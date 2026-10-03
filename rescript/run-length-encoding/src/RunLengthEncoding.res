// An exercise in extracting options:
// The result of String.match when using a regexp with "g" flag
// is: option<array<option<string>>>

let encode = plaintext => {
  switch plaintext->String.match(/(.)\1*/g) {
  | None => ""
  | Some(result) =>
    result
    ->Array.keepSome
    ->Array.map(run => {
      switch String.length(run) {
      | 1 => ""
      | len => Int.toString(len)
      } ++
      String.charAt(run, 0)
    })
    ->Array.join("")
  }
}

let decode = ciphertext => {
  switch ciphertext->String.match(/\d*\D/g) {
  | None => ""
  | Some(result) =>
    result
    ->Array.keepSome
    ->Array.map(encoded => {
      switch String.length(encoded) {
      | 1 => encoded
      | len => {
          let char = encoded->String.charAt(len - 1)
          let n = encoded->String.substring(~start=0, ~end=len - 1)
          String.repeat(char, Option.getOr(Int.fromString(n), 0))
        }
      }
    })
    ->Array.join("")
  }
}
