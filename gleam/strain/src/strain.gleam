import gleam/list

pub fn keep(list: List(t), predicate: fn(t) -> Bool) -> List(t) {
  strain(list, predicate, [])
}

pub fn discard(list: List(t), predicate: fn(t) -> Bool) -> List(t) {
  strain(list, fn(elem) { !predicate(elem) }, [])
}

fn strain(list, pred, kept) {
  case list {
    [] -> list.reverse(kept)
    [elem, ..rest] ->
      case pred(elem) {
        True -> strain(rest, pred, [elem, ..kept])
        False -> strain(rest, pred, kept)
      }
  }
}
