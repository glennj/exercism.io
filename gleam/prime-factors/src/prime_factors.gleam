import gleam/list

pub fn factors(value: Int) -> List(Int) {
  do_factors(value, 2, [])
}

fn do_factors(n, f, fs) {
  // an optimization: this only loops up to √n
  case f * f > n, n == 1 {
    True, True -> list.reverse(fs)
    True, False -> list.reverse([n, ..fs])
    False, _ ->
      case n % f, f {
        0, _ -> do_factors(n / f, f, [f, ..fs])
        _, 2 -> do_factors(n, f + 1, fs)
        _, _ -> do_factors(n, f + 2, fs)
      }
  }
}
