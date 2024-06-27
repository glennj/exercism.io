import gleam/int
import gleam/list

// ------------------------------
// the mathematical answer:

// pub fn square_of_sum(n: Int) -> Int {
//   let m = n * { n + 1 } / 2
//   m * m
// }

// pub fn sum_of_squares(n: Int) -> Int {
//   n * { n + 1 } * { 2 * n + 1 } / 6
// }

// ------------------------------
// or, a more fun functional one:

type IntFn = fn(Int) -> Int

fn square(n: Int) -> Int { n * n }
fn ident(n: Int)  -> Int { n }

fn sum(n: Int, inner: IntFn, outer: IntFn) -> Int {
  list.range(1, n)
  |> list.map(inner)
  |> int.sum
  |> outer
}

pub fn square_of_sum(n: Int)  -> Int { sum(n, ident, square) }
pub fn sum_of_squares(n: Int) -> Int { sum(n, square, ident) }

pub fn difference(n: Int) -> Int {
  sum_of_squares(n) - square_of_sum(n) |> int.absolute_value
}
