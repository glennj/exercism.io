import gleam/list
import gleam/string

pub fn slices(input: String, size: Int) -> Result(List(String), Error) {
  let len = string.length(input)
  case len, size {
    0, _ -> Error(EmptySeries)
    _, 0 -> Error(SliceLengthZero)
    _, s if s < 0 -> Error(SliceLengthNegative)
    _, s if s > len -> Error(SliceLengthTooLarge)
    _, _ -> string.to_graphemes(input) |> list.window(size) |> list.map(string.join(_, "")) |> Ok
  }
}

pub type Error {
  EmptySeries
  SliceLengthNegative
  SliceLengthTooLarge
  SliceLengthZero
}
