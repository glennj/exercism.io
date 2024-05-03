// not tail recursive
pub fn accumulate(list: List(a), fun: fn(a) -> b) -> List(b) {
  case list {
    [] -> []
    [head, ..tail] -> [fun(head), ..accumulate(tail, fun)]
  }
}
