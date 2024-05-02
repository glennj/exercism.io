import gleam/set.{type Set}
import gleam/string

pub fn is_isogram(phrase phrase: String) -> Bool {
  do_is_isogram(clean_phrase(phrase), set.new())
}

fn clean_phrase(phrase) -> List(String) {
  phrase
  |> string.lowercase()
  |> string.replace(" ", "")
  |> string.replace("-", "")
  |> string.to_graphemes()
}

fn do_is_isogram(chars: List(String), seen: Set(String)) -> Bool {
  case chars {
    [] -> True
    [c, ..cs] ->
      case set.contains(seen, c) {
        True -> False
        False -> do_is_isogram(cs, set.insert(seen, c))
      }
  }
}