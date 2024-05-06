import gleam/io

fn factorial(n: Int) -> Int {
  case n {
    1 -> 1
    _ -> n * factorial(n - 1)
  }
}

pub fn main() {
  io.debug(factorial(1))
  io.debug(factorial(2))
  io.debug(factorial(3))
  io.debug(factorial(4))
  io.debug(factorial(5))
  io.debug(factorial(6))
  io.debug(factorial(7))
  io.debug(factorial(8))
  io.debug(factorial(9))
  io.debug(factorial(10))
}
