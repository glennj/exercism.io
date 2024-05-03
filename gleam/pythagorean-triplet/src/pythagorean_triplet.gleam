import gleam/list

pub type Triplet {
  Triplet(Int, Int, Int)
}

pub fn triplets_with_sum(sum: Int) -> List(Triplet) {
  // magic number = shortest side of smallest Pythagorean triplet [3, 4, 5]
  find_triplets(3, sum, [])
}

fn find_triplets(a: Int, n: Int, triplets: List(Triplet)) -> List(Triplet) {
  let b = n * {n - 2 * a} / {2 * {n - a}}
  case a >= b {
    True -> triplets |> list.reverse()
    False -> {
      let c = n - a - b
      case a * a + b * b == c * c {
        True -> find_triplets(a + 1, n, [Triplet(a, b, c), ..triplets])
        False -> find_triplets(a + 1, n, triplets)
      }
    }
  }
}
