import gleam/string

pub fn reverse(value: String) -> String {
  do_reverse(value, "")
}

fn do_reverse(s, r) {
  case string.pop_grapheme(s) {
    Error(Nil) -> r
    Ok(#(c, rest)) -> do_reverse(rest, c <> r)
  }
}