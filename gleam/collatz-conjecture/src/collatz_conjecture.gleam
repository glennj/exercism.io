import gleam/int

pub type Error {
  NonPositiveNumber
}

pub fn steps(number: Int) -> Result(Int, Error) {
  case number {
    n if n <= 0 -> Error(NonPositiveNumber)
    _ -> Ok(do_collatz(number, 0))
  }
}

fn do_collatz(num, steps) -> Int {
  case num, int.is_even(num) {
    1, _ -> steps
    _, True -> do_collatz(num / 2, steps + 1)
    _, False -> do_collatz(3 * num + 1, steps + 1)
  }
}
