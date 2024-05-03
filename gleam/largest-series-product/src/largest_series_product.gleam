import gleam/int
import gleam/list
import gleam/string

pub fn largest_product(digit_string: String, span: Int) -> Result(Int, Nil) {
  case span, string.length(digit_string), int.parse(digit_string) {
    0, _, _ -> Ok(1)
    _, 0, _ -> Error(Nil)
    sp, len, _ if sp < 0 || sp > len -> Error(Nil)
    _, _, Error(_) -> Error(Nil)
    _, _, Ok(number) ->
      case int.digits(number, 10) {
        Error(_) -> Error(Nil)
        Ok(digits) ->
          digits
          |> list.window(span)
          |> list.map(int.product)
          |> list.fold(0, int.max)
          |> Ok()
      }
  }
}
