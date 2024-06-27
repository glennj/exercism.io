import gleam/io
import gleam/list
import gleam/regex

fn factorial(n: Int) -> Int {
  case n {
    1 -> 1
    _ -> n * factorial(n - 1)
  }
}

pub fn main() {
  let assert Ok(re) = regex.from_string("(^)(xr.*|yt.*)")
  io.debug(factorial(10))
  io.debug(list.length(list.permutations([1,2,3,4,5])))

  io.debug(regex.scan(re, "xray"))
}
