let nanpPattern = /^1?([2-9]\d\d)([2-9]\d\d)(\d{4})$/
let validNonDigits = /[() .+-]/g

let clean = input =>
  switch input
         ->String.replaceAllRegExp(validNonDigits, "")
         ->String.match(nanpPattern)
  {
  | None => None
  | Some(result) =>
    result
    ->Array.keepSome
    ->Array.slice(~start=1)
    ->Array.join("")
    ->Some
  }
