import gleam/list

type Row = List(Int)
type Triangle = List(Row)

pub fn rows(n: Int) -> Triangle {
  case n {
    0 -> []
    1 -> [[1]]
    _ -> build_rows(n, 2, rows(1))
  }
}

fn build_rows(n: Int, row: Int, triangle: Triangle) -> Triangle {
  case row {
    r if r > n -> triangle
    _ -> build_rows(n, row + 1, list.append(triangle, build_row(n, row, 0, [])))
  }
}

fn build_row(n: Int, i: Int, j: Int, row: Row) -> Triangle {
  case j {
    c if c == n -> [row]
    _ -> build_row(n, i, j + 1, [n_choose_k(i, j + 1), ..row])
  }
}

fn n_choose_k(n: Int, k: Int) -> Int {
  factorial(n) / factorial(k) / factorial(n - k)
}

// TODO memoize, somehow.
fn factorial(n: Int) -> Int {
  case n {
    1 -> 1
    _ -> n * factorial(n - 1)
  }
}
