import gleam/list
import gleam/option
import gleam/regex.{type Regex}
import gleam/string

pub fn translate(phrase: String) -> String {
  phrase
  |> string.split(" ")
  |> list.map(translate_word)
  |> string.join(" ")
}

fn translate_word(word: String) -> String {
  // each regex has 2 sets of capturing parens.
  // they are all anchored, so at most one match per scan.
  let assert Ok(xr) = regex.from_string("^()(xr.*|yt.*)")
  let assert Ok(qu) = regex.from_string("^([^aeiou]*qu)(.*)")
  let assert Ok(y) = regex.from_string("^([^aeiou]+)(y.*)")
  let assert Ok(cs) = regex.from_string("^([^aeiou]+)(.*)")
  pigify(word, [xr, qu, y, cs])
}

fn pigify(word: String, regexes: List(Regex)) -> String {
  case regexes {
    [] -> word <> "ay"
    [re, ..rest] ->
      case regex.scan(re, word) {
        [match] -> {
          let assert [s1, s2] =
            list.map(match.submatches, fn(sm) { option.unwrap(sm, "") })
          s2 <> s1 <> "ay"
        }
        _ -> pigify(word, rest)
      }
  }
}
