# Erlang Extraction

Welcome to Erlang Extraction on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## External Functions

Gleam can run on the Erlang virtual machine (BEAM), or on JavaScript runtimes. There are many other languages that use these runtimes, and it is often useful to be able to call code written in these languages from Gleam.

Gleam's _external functions_ feature permits functions in other languages to be imported into Gleam and called with no runtime overhead.

If your Gleam project runs on the Erlang virtual machine and you wish to call the `reverse` function from the Erlang `lists` module you can do it by adding the `@external` attribute to a Gleam function head like this:

```gleam
@external(erlang, "lists", "reverse")
pub fn reverse_list(x: List(a)) -> List(a)
```

This can then be called as a normal Gleam function:

```gleam
let reversed = reverse_list([1, 2, 3])
// -> [3, 2, 1]
```

If you attempt to compile this code for JavaScript runtimes it will fail with an error message as there is no implementation for JavaScript. Another implementation can be specified for JavaScript runtimes like this:

```gleam
@external(erlang, "lists", "reverse")
@external(javascript, "./my_module.mjs", "reverse")
pub fn reverse_list(x: List(a)) -> List(a)
```

It is also possible to write a Gleam implementation that will be used when there is no external implementation for the current compile target:

```gleam
@external(erlang, "lists", "reverse")
pub fn reverse_list(x: List(a)) -> List(a) {
  tail_recursive_reverse(x, [])
}

fn tail_recursive_reverse(list, reversed) {
  case list {
    [] -> reversed
    [x, ..xs] -> tail_recursive_reverse(xs, [x, ..reversed])
  }
}
```

## External Types

External types can be used to refer to data types defined in other languages, such as Erlang or JavaScript.

To define an external function declare a type but do not provide any constructors. This can then be used in the same way as any other type.

```gleam
pub type OrderedDictionary(element)
```

## Instructions

Tadhg has found the perfect Erlang library to help with his project. Being an older Erlang library it is using the `gb_trees` module rather than the newer `maps` module for storing data.

Help out Tadgh by creating external types and functions for working with `gb_trees` in Gleam.

## 1. Define the `GbTree` external type

The `GbTree` type should have two type parameters, a key type and a value type. It should have no constructors, making it an external type.

## 2. Define the `new_gb_tree` function

The `new_gb_tree` function should take no arguments and return an empty `GbTree`.

It should use the `gb_trees:empty/0` function from the Erlang standard library.

## 3. Define the `insert` function

The `insert` function should take a `GbTree` and a key and value to insert into the tree. It should return a new `GbTree` with the key and value inserted.

The function should take three arguments:
1. The `GbTree` to insert into.
2. The key to insert.
3. The value to insert.

It should use the `gb_trees:insert/3` function from the Erlang standard library.

## 4. Define the `delete` function

The `delete` function should take a `GbTree` and a key to delete from the tree. It should return a new `GbTree` with the key and value deleted.

The function should take two arguments:
1. The `GbTree` to delete from.
2. The key to delete.

It should use the `gb_trees:delete_any/2` function from the Erlang standard library.

## Source

### Created by

- @lpil