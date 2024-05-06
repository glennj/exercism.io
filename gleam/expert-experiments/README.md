# Expert Experiments

Welcome to Expert Experiments on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Use Expressions

In Gleam it is common to write and use higher order functions, that is functions that take other functions as arguments. Sometimes when using many higher order functions at once the code can become difficult to read, with many layers of indentation.

For example, here is a function that calls several functions that return `Result(Int, Nil)`, and sums the values if all four are successful.

```gleam
import gleam/result

pub fn main() -> Result(Int, Nil) {
  result.try(function1(), fn(a) {
    result.try(function2(), fn(b) {
      result.try(function3(), fn(c) {
        result.try(function4(), fn(d) {
          Ok(a + b + c + d)
        })
      })
    })
  })
}
```

Gleam's `use` expressions allow us to write this code without the indentation, often making it easier to read.

```gleam
import gleam/result

pub fn main() -> Result(Int, Nil) {
  use a <- result.try(function1())
  use b <- result.try(function2())
  use c <- result.try(function3())
  use d <- result.try(function4())
  Ok(a + b + c + d)
}
```

A `use` expression collects all the following statements in the block and passes it as a callback function as the final argument to the function call. The variables between the `use` keyword and the `<-` symbol are the names of the arguments that will be passed to the callback function.

```gleam
// This use expression
use a <- function(1, 2)
io.println("Hello!")
a

// Is equivalent to this normal function call
function(1, 2, fn(a) {
  io.println("Hello!")
  a
})
```

The callback function can take any number of arguments, or none at all.

```gleam
use a, b, c, d <- call_4_function()

use <- call_0_function()
```

There are no special requirements to create a function that can be called with a `use` expression, other than taking a callback function as the final argument.

```gleam
pub fn call_twice(function: fn() -> t) -> #(t, t) {
  let first = function()
  let second = function()
  #(first, second)
}
```

Gleam's `use` expressions are a very powerful feature that can be applied to lots of problems, but when overused they can make code difficult to read. It is generally preferred to use the normal function call syntax and only reach for `use` expressions when they make the code easier to read.

## Instructions

Daphne has been working on a system to run and record the results of her experiments. Some of the code has become a bit verbose and repetitive, so she's asked you to write some `use` expressions to help clean it up.

## 1. Define the `with_retry` function

Sometimes experiments can fail due to a one-off mistake, so if an experiment fails Daphne wants to retry it again to see if it works the second time.

Define the `with_retry` function that takes a result returning function as an argument.

If the function returns an `Ok` value then `with_retry` should return that value.

If the function returns an `Error` value then `with_retry` should call the function again and return the result of that call.

Daphne will use the function like this:

```gleam
pub fn main() {
  use <- with_retry
  // Perform the experiment here
}
```

## 2. Define the `record_timing` function

Daphne records how long each experiment takes to run by calling a time logging function before and after each experiment.

Define the `record_timing` function that takes two arguments:
- A time logging function which takes no arguments and returns `Nil`.
- An experiment function which takes no arguments and returns a result.

`record_timing` should call the time logging function, then call the experiment function, then call the time logging function again, and finally return the result of the experiment function.

Daphne will use the function like this:

```gleam
pub fn main() {
  use <- record_timing(time_logger)
  // Perform the experiment here
}
```

## 3. Define the `run_experiment` function

Experiments are made up of three phases. The setup, the action, and the recording. All three phases return results, and each phase needs the successful result of the previous phase to run.

Define the `run_experiment` function that takes four arguments:
- The name of the experiment as a `String`.
- A setup function which takes no arguments and returns a result.
- An action function which takes the `Ok` value of the setup function as an argument and returns a result.
- A recording function which takes the `Ok` value of the setup and action functions as arguments and returns a result.

If all three functions succeed then `run_experiment` should return `Ok(#(experiment_name, recording_data))`.

If any of the functions return an `Error` value then `run_experiment` should return that value.

Daphne will use the function like this:

```gleam
pub fn main() {
  use setup_data, action_data <- run_experiment("Test 1", setup, action)
  // Record the results here
}
```

## Source

### Created by

- @lpil