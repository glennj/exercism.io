// Using the Binary numeral system implementation from Wikipedia
//  https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)

pub fn square_root(radicand: Int) -> Int {
  do_sqrt(radicand, 0, get_b(radicand, 1))
}

// find the largest power of 4 less than or equal to n
fn get_b(n, b) {
  case b <= n {
    True -> get_b(n, b * 4)
    False -> b / 4
  }
}

fn do_sqrt(n, x, b) {
  case b == 0 {
    True -> x
    False ->
      case n >= b + x {
        True -> do_sqrt(n - x - b, x / 2 + b, b / 4)
        False -> do_sqrt(n, x / 2, b / 4)
      }
  }
}
