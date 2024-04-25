pub type Pizza {
  Margherita
  Caprese
  Formaggio
  ExtraSauce(Pizza)
  ExtraToppings(Pizza)
}

pub fn pizza_price(pizza: Pizza) -> Int {
  do_pizza_price(pizza, 0)
}

fn do_pizza_price(pizza, cost) {
  case pizza {
    Margherita -> cost + 7
    Caprese -> cost + 9
    Formaggio -> cost + 10
    ExtraSauce(p) -> do_pizza_price(p, cost + 1)
    ExtraToppings(p) -> do_pizza_price(p, cost + 2)
  }
}

pub fn order_price(order: List(Pizza)) -> Int {
  do_order_price(order, 0, 0)
}

fn do_order_price(order, cost, count) {
  case order {
    [] -> cost + order_fee(count)
    [pizza, ..rest] ->
      do_order_price(rest, cost + pizza_price(pizza), count + 1)
  }
}

fn order_fee(num_pizzas) {
  case num_pizzas {
    1 -> 3
    2 -> 2
    _ -> 0
  }
}
