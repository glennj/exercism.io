import gleam/int
import gleam/option.{Some}
import gleam/regex.{type Regex}
import gleam/string

// -----------------------------------------------------------
/// encoding with recursion, iterating one character at a time
///
pub fn encode(plaintext: String) -> String {
  case string.pop_grapheme(plaintext) {
    Error(Nil) -> ""
    Ok(#(char, rest)) -> do_encode(rest, char, 1, "")
  }
}

fn do_encode(text: String, prev: String, count: Int, encoded: String) -> String {
  let add_run = fn(str, num, letter) {
    str <> case num {
      1 -> letter
      _ -> int.to_string(num) <> letter
    }
  }

  case string.pop_grapheme(text) {
    Error(Nil) -> add_run(encoded, count, prev)
    Ok(#(letter, rest)) if letter == prev -> do_encode(rest, prev, count + 1, encoded)
    Ok(#(letter, rest)) -> do_encode(rest, letter, 1, add_run(encoded, count, prev))
  }
}

// -----------------------------------------------------------
/// decoding with a regular expression
///
pub fn decode(ciphertext: String) -> String {
  // this pattern matches the *last* run
  // - does not handle the invalid case where the ciphertext ends with a digit
  let assert Ok(re) = regex.from_string("^(.*?)(\\d+)(\\D)(\\D*)$")
  do_decode(ciphertext, re)
}

fn do_decode(text: String, re: Regex) -> String {
  // because the pattern is anchored at both ends, we'll get at most one match.
  case regex.scan(re, text) {
    [] -> text
    [match, ..] -> {
      let assert [prefix, Some(digits), Some(char), suffix] = match.submatches
      let assert Ok(len) = int.parse(digits)
      let text = option.unwrap(prefix, "") <> string.repeat(char, len) <> option.unwrap(suffix, "")
      do_decode(text, re)
    }
  }
}
