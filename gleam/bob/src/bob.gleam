import gleam/regex
import gleam/string

pub fn hey(remark: String) -> String {
  let trimmed = string.trim(remark)
  let silent = string.is_empty(trimmed)
  let ask = string.ends_with(trimmed, "?")

  // let yell = case regex.from_string("^[^[:lower:]]*[[:upper:]][^[:lower:]]*$") {
  //   Ok(re) -> regex.check(re, trimmed)
  //   _ -> False
  // }
  let assert Ok(re) = regex.from_string("^[^[:lower:]]*[[:upper:]][^[:lower:]]*$")
  let yell = regex.check(re, trimmed)

  case silent, ask, yell {
    True, _, _    -> "Fine. Be that way!"
    _, True, True -> "Calm down, I know what I'm doing!"
    _, True, _    -> "Sure."
    _, _, True    -> "Whoa, chill out!"
    _, _, _       -> "Whatever."
  }
}
