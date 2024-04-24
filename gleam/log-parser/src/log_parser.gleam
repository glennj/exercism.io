import gleam/regex as re
import gleam/string.{trim}

pub fn is_valid_line(line: String) -> Bool {
  let assert Ok(r) = re.from_string("^\\[(?:DEBUG|INFO|WARNING|ERROR)\\]")
  re.check(r, line)
}

pub fn split_line(line: String) -> List(String) {
  let assert Ok(r) = re.from_string("<[-=~*]*>")
  re.split(r, line)
}

pub fn tag_with_user_name(line: String) -> String {
  // using a look-behind. must be fixed-width
  let assert Ok(r) = re.from_string("(?<=\\bUser\\b)\\s+\\S+")
  case re.scan(r, line) {
    [re.Match(content: name, ..)] -> "[USER] " <> trim(name) <> " " <> line
    _ -> line
  }
}
