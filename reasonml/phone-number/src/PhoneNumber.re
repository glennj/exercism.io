let phoneNumber = input => {
  let validNonDigits = Core.RegExp.fromString("/[() .+-]/g")
  let validNumber = Core.RegExp.fromString("/^1?([2-9]\\d\\d)([2-9]\\d\\d)(\\d{4})$?")
  let cleaned = Core.String.replaceRegExp(input, validNonDigits, "")

  switch (Js.String.match(cleaned, validNumber)) {
  | None => None
  | Some(m) => 
      switch (Core.RegExp.Result.matches(m)) {
      | [area, exchange, number] => Some(area ++ exchange ++ number)
      | _ => None
      }
  }
};
