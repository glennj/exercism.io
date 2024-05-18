import gleam/list
import gleam/string

pub fn is_paired(value: String) -> Bool {
  check(value, [])
}

fn check(text, stack) {
  case string.pop_grapheme(text) {
    Error(Nil) -> list.is_empty(stack)
    Ok(#(char, rest_of_text)) ->
      case is_open_bracket(char), is_close_bracket(char) {
        True, _ -> check(rest_of_text, [char, ..stack])
        False, False -> check(rest_of_text, stack)
        False, True ->
          case stack {
            [] -> False
            [opener, ..rest_of_stack] ->
              case is_pair(opener, char) {
                True -> check(rest_of_text, rest_of_stack)
                False -> False
              }
          }
      }
  }
}

fn is_open_bracket(b) {
  ["[", "{", "("] |> list.contains(b)
}

fn is_close_bracket(b) {
  ["]", "}", ")"] |> list.contains(b)
}

fn is_pair(a, b) {
  case a {
    "[" -> b == "]"
    "{" -> b == "}"
    "(" -> b == ")"
    _ -> False
  }
}
