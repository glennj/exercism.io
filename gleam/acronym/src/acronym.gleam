import gleam/order.{Eq, Gt, Lt}
import gleam/string

pub fn abbreviate(phrase phrase: String) -> String {
  abbrev(string.uppercase(phrase), True, "")
}

/// This is a little state machine.
/// When `seeking_alpha` is True, we're looking for the next letter.
/// - If we find one, add it the the acronym, and look for a non-letter.
/// - Otherwise, continue looking for a letter.
/// When `seeking_alpha` is False, we're looking for the next non-letter.
/// - If we see a letter _or an apostrophe_, continue looking for the next non-letter.
/// - Otherwise, start looking for a letter
///
fn abbrev(text, seeking_alpha, acronym) {
  case string.pop_grapheme(text) {
    Error(Nil) -> acronym
    Ok(#(char, rest)) ->
      case seeking_alpha, is_letter(char), is_apostrophe(char) {
        True, True, _ -> abbrev(rest, False, acronym <> char)
        True, False, _ -> abbrev(rest, True, acronym)
        False, True, _ -> abbrev(rest, False, acronym)
        False, False, True -> abbrev(rest, False, acronym)
        False, False, False -> abbrev(rest, True, acronym)
      }
  }
}

fn is_letter(char: String) -> Bool {
  case string.compare(char, "A"), string.compare(char, "Z") {
    Eq, Lt | Gt, Lt | Gt, Eq -> True
    _, _ -> False
  }
}

fn is_apostrophe(char: String) -> Bool {
  case char {
    "'" -> True
    _ -> False
  }
}
