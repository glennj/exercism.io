import gleam/bool
import gleam/int
import gleam/list
import gleam/string

pub fn is_valid(isbn: String) -> Bool {
  let digits = prepare_digits(isbn)
  use <- bool.guard(when: list.length(digits) != 10, return: False)
  case isbn_sum(digits) {
    Ok(sum) -> sum % 11 == 0
    _ -> False
  }
}

fn prepare_digits(isbn: String) -> List(String) {
  isbn
  |> string.replace("-", "")
  |> string.reverse()
  |> string.to_graphemes()
  |> list.index_map(fn(grapheme, index) {
    case index, grapheme {
      0, "X" -> "10"
      _, _ -> grapheme
    }
  })
}

fn isbn_sum(digits: List(String)) -> Result(Int, Nil) {
  digits
  |> list.index_map(fn(d, i) { #(d, i) })
  |> list.fold_until(Ok(0), fn(acc, item) {
    let assert Ok(sum) = acc
    case int.parse(item.0) {
      Ok(digit) -> list.Continue(Ok(sum + { item.1 + 1 } * digit))
      _ -> list.Stop(Error(Nil))
    }
  })
}
