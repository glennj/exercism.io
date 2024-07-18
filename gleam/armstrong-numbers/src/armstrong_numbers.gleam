import gleam/int
import gleam/list
import gleam/result

pub fn is_armstrong_number(number: Int) -> Bool {
  let assert Ok(width) = int.digits(number, 10) |> result.map(list.length)
  number == armstrong_sum(number, width, 0)
}

fn armstrong_sum(number: Int, width: Int, sum: Int) -> Int {
  case number {
    0 -> sum
    _ -> armstrong_sum(number / 10, width, sum + ipow(number % 10, width))
  }
}

fn ipow(n: Int, e: Int) -> Int {
  case e {
    0 -> 1
    _ -> n * ipow(n, e - 1)
  }
}