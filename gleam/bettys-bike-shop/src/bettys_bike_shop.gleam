import gleam/int
import gleam/float
// import gleam/string

pub fn pence_to_pounds(pence: Int) -> Float {
  int.to_float(pence) /. 100.
}

pub fn pounds_to_string(pounds: Float) -> String {
  "£" <> float.to_string(pounds)
}
