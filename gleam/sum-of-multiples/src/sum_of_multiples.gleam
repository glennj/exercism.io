import gleam/int
import gleam/list
import gleam/set.{type Set}

pub fn sum(factors factors: List(Int), limit limit: Int) -> Int {
  list.filter(factors, fn(f) { f > 0 })
  |> collect_multiples(set.new(), _, limit)
  |> set.to_list()
  |> int.sum()
}

fn collect_multiples(
  multiples: Set(Int),
  factors: List(Int),
  limit: Int,
) -> Set(Int) {
  case factors {
    [] -> multiples
    [factor, ..rest] ->
      collect_multiples(
        add_multiples(multiples, factor, factor, limit),
        rest,
        limit,
      )
  }
}

fn add_multiples(
  multiples: Set(Int),
  multiple: Int,
  factor: Int,
  limit: Int,
) -> Set(Int) {
  case multiple {
    m if m >= limit -> multiples
    _ ->
      add_multiples(
        set.insert(multiples, multiple),
        multiple + factor,
        factor,
        limit,
      )
  }
}
