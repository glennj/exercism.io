# Secrets

Welcome to Secrets on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Anonymous Functions

Gleam has first class functions, meaning functions are normal values that can be assigned to variables, passed as arguments, and returned from other functions.

A named function defined in a module can be referenced by its name.

```gleam
pub fn main() {
  // Assign the function to a variable
  let f = add_one
  
  // Invoke the function
  f(1) // -> 2
  f(2) // -> 3
}

fn add_one(x) {
  x + 1
}
```

Gleam also has anonymous functions, which are defined within other functions.

```gleam
// Define the function
let f = fn(x) { x + 1 }

// Invoke the function
f(1) // -> 2
f(2) // -> 3
```

The type of a function is written using a similar syntax to anonymous functions. The type of a function that takes an `Int` and a `Float` and returns an `Int` is written like this:

```gleam
fn(Int, Float) -> Int
```

Anonymous functions can reference variables that were in scope when they were defined, making them _closures_.

```gleam
let secret_number = 42

// This function always returns 42
fn() { secret_number }
```

The _function capture_ syntax provides a convenient shorthand for creating anonymous functions that pass a single argument to a function. These two expressions are equivalent:

```gleam
// Anonymous function syntax
let f = fn(x) { my_function(1, 2, x) }

// Function capture syntax
let f = my_function(1, 2, _)
```

## Instructions

In this exercise, you've been tasked with writing the software for an encryption device that works by performing transformations on data. You need a way to flexibly create complicated functions by combining simpler functions together.

For each task, return an anonymous function that can be invoked from the calling scope.

## 1. Create an adder

Implement `secret_add`. It should return a function which takes one argument and adds to it the argument passed in to `secret_add`.

```gleam
let adder = secret_add(2)
adder(2)
// -> 4
```

## 2. Create a subtractor

Implement `secret_subtract`. It should return a function which takes one argument and subtracts the secret passed in to `secret_subtract` from that argument.

```gleam
let subtractor = secret_subtract(2)
subtractor(3)
// -> 1
```

## 3. Create a multiplier

Implement `secret_multiply`. It should return a function which takes one argument and multiplies it by the secret passed in to `secret_multiply`.

```gleam
let multiplier = secret_multiply(7)
multiplier(3)
// -> 21
```

## 4. Create a divider

Implement `secret_divide`. It should return a function which takes one argument and divides it by the secret passed in to `secret_divide`.

```gleam
let divider = secret_divide(3)
divider(32)
// -> 10
```

## 5. Create a function combiner

Implement `secret_combine`. It should return a function which takes one argument and applies to it the two functions passed in to `secret_combine` in order.

```gleam
let multiply = secret_multiply(7)
let divide = secret_divide(3)
let combined = secret_combine(multiply, divide)

combined(6)
// -> 14
```

## Source

### Created by

- @lpil