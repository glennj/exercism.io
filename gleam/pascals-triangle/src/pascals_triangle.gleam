import gleam/list

type Row =
  List(Int)

type Triangle =
  List(Row)

pub fn rows(n: Int) -> Triangle {
  construct(n, 0, [])
}

fn construct(n: Int, i: Int, triangle: Triangle) -> Triangle {
  case i >= n {
    True -> triangle
    False -> construct(n, i + 1, list.append(triangle, one_row(i, 0, [])))
  }
}

fn one_row(i: Int, j: Int, row: Row) -> Triangle {
  case j > i {
    True -> [row]
    False -> one_row(i, j + 1, [n_choose_k(i, j), ..row])
  }
}

fn n_choose_k(n: Int, k: Int) -> Int {
  factorial(n) / factorial(k) / factorial(n - k)
}

// TODO memoize, somehow.
fn factorial(n: Int) -> Int {
  case n {
    0 -> 1
    1 -> 1
    _ -> n * factorial(n - 1)
  }
}
