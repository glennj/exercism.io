import gleam/int

pub fn egg_count(n: Int) -> Int {
  case n {
    0 -> 0
    _ -> int.bitwise_and(n, 1) + egg_count(int.bitwise_shift_right(n, 1))
  }
}
