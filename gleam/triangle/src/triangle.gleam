import gleam/list
import gleam/float

/// Return a tuple of 2 values:
/// * (Bool) is it a valid triangle
/// * (Int) the number of unique side lengths
fn validate_and_classify(a: Float, b: Float, c: Float) -> #(Bool, Int) {
  let assert [a, b, c] = list.sort([a, b, c], float.compare)
  #(
    a >. 0.0 && a +. b >. c,
    list.unique([a, b, c]) |> list.length()
  )
}

pub fn equilateral(a: Float, b: Float, c: Float) -> Bool {
  let #(valid, n) = validate_and_classify(a, b, c)
  valid && n == 1
}

pub fn isosceles(a: Float, b: Float, c: Float) -> Bool {
  let #(valid, n) = validate_and_classify(a, b, c)
  valid && n <= 2
}

pub fn scalene(a: Float, b: Float, c: Float) -> Bool {
  let #(valid, n) = validate_and_classify(a, b, c)
  valid && n == 3
}
