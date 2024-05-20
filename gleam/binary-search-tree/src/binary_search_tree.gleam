import gleam/list

pub type Tree {
  Nil
  Node(data: Int, left: Tree, right: Tree)
}

pub fn to_tree(data: List(Int)) -> Tree {
  add_all(Nil, data)
}

fn add_all(tree: Tree, data: List(Int)) -> Tree {
  case data {
    [] -> tree
    [item, ..rest] -> tree |> insert(item) |> add_all(rest)
  }
}

fn insert(tree: Tree, item: Int) -> Tree {
  case tree {
    Nil -> Node(item, Nil, Nil)
    Node(value, left, right) ->
      case item <= value {
        True -> Node(data: value, right: right, left: insert(left, item))
        False -> Node(data: value, left: left, right: insert(right, item))
      }
  }
}

pub fn sorted_data(data: List(Int)) -> List(Int) {
  data |> to_tree() |> in_order()
}

fn in_order(tree: Tree) -> List(Int) {
  case tree {
    Nil -> []
    Node(item, left, right) ->
      list.concat([in_order(left), [item], in_order(right)])
  }
}
