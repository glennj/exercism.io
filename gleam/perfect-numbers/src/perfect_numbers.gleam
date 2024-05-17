import gleam/float
import gleam/int
import gleam/order
import gleam/set.{type Set}

pub type Classification {
  Perfect
  Abundant
  Deficient
}

pub type Error {
  NonPositiveInt
}

pub fn classify(number: Int) -> Result(Classification, Error) {
  case number < 1 {
    True -> Error(NonPositiveInt)
    False -> {
      case int.compare(aliquot_sum(number), number) {
        order.Lt -> Ok(Deficient)
        order.Eq -> Ok(Perfect)
        order.Gt -> Ok(Abundant)
      }
    }
  }
}

fn aliquot_sum(n: Int) -> Int {
  do_aliquot_sum(n, isqrt(n), set.new())
}

fn do_aliquot_sum(n: Int, f: Int, factors: Set(Int)) -> Int {
  case f == 0 {
    True -> set.fold(factors, -n, fn(sum, factor) { sum + factor })
    False ->
      case n % f == 0 {
        True ->
          do_aliquot_sum(
            n,
            f - 1,
            factors
              |> set.insert(f)
              |> set.insert(n / f),
          )
        False -> do_aliquot_sum(n, f - 1, factors)
      }
  }
}

fn isqrt(n: Int) -> Int {
  let assert Ok(sqrt) = int.square_root(n)
  float.truncate(sqrt)
}
