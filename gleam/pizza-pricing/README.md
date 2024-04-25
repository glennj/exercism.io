# Pizza Pricing

Welcome to Pizza Pricing on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Tail Call Optimisation

Each time a function is called a new stack frame is created in memory to store the arguments and local variables of the function, and this stack frame is deallocated when the function returns. Because Gleam uses recursive function calls instead of a looping syntax this can result in a large amount of memory being used by these stack frames.

To avoid this problem Gleam supports tail call optimisation, which allows the compiler to reuse the stack frame for the current function if a function call is the last thing the function does. This means that a function can call itself an infinite number of times without using any additional memory.

Unoptimised recursive functions can often be rewritten into tail call optimised functions by using an _accumulator_.

An accumulator is a variable that is passed along in addition to the data. It is used to pass the current state of the function's execution, until the base case is reached.

Accumulators should be initialized by the function's author, not the function's user. To achieve this, declare two functions - a public function that takes just the necessary data as arguments and initializes the accumulator, and a private function that also takes an accumulator.

```gleam
// Count the length of a list without tail call optimisation
pub fn count(list: List(String)) -> Int {
  case list {
    [] -> 0
    [_, ..rest] -> {
      let amount = count(rest) // Non-tail recursive call
      amount + 1
    }
  }
}
```

```gleam
// Count the length of a list with tail call optimisation
pub fn count(list: List(String)) -> Int {
  count_elements(list, 0)
}

fn count_elements(list: List(String), accumulator: Int) -> Int {
  case list {
    [] -> accumulator
    [_, ..rest] -> {
      let accumulator = accumulator + 1
      count_elements(rest, accumulator) // Tail recursive call
    }
  }
}
```

## Instructions

In this exercise you're working at a pizza place that delivers to customers.

You offer three types of pizzas:

- Margherita: \$7
- Caprese: \$9
- Formaggio: \$10

Customers can also choose two additional options for a small additional fee:

1. Extra sauce: \$1
1. Extra toppings: \$2

When customers place an order, an additional fee is added if they only order one or two pizzas:

- 1 pizza: \$3
- 2 pizzas: \$2

You have three tasks, each of which will work with pizzas and their price.

## 1. Define the pizza types and options

Define the `Pizza` custom type to represent the different types of pizzas and options:

- `Margherita`
- `Caprese`
- `Formaggio`
- `ExtraSauce`
- `ExtraToppings`

`ExtraSauce` and `ExtraToppings` should wrap the `Pizza` type.

## 2. Calculate the price of pizza

Implement the `pizza_price` function to calculate a pizza's price:

```gleam
pizza_price(Caprese)
// -> 9
```

## 3. Calculate the price of an order

Implement the `order_price` function to calculate a pizza order's price:

```gleam
order_price([Margherita, Formaggio])
// -> 19
```

## Source

### Created by

- @lpil