import gleam/io
import gleam/list

fn factorial(n: Int) -> Int {
  case n {
    1 -> 1
    _ -> n * factorial(n - 1)
  }
}

pub fn main() {
  io.debug(factorial(10))
  io.debug(list.length(list.permutations([1,2,3,4,5])))
}
