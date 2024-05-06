# Magician in Training

Welcome to Magician in Training on Exercism's Gleam Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Queues

The Gleam standard library implements a `Queue` type in the `gleam/queue` module. It is similar to the `List` type, but with a few key differences:

- The `Queue` type doesn't have a literal syntax that can be used to construct queues or pattern match on them.
- Elements can be efficiently added and removed from both the front and back of the `Queue` type, while the `List` type can only efficiently add and remove elements from the front.

Most of the time you will want to use the `List` type, but when you need to be able to add and remove from the end of a collection, the `Queue` type is a good choice.

Queues can be created using the `queue.new` and `queue.from_list` functions:

```gleam
let empty = queue.new()
let one_to_four = queue.from_list([1, 2, 3, 4])
```

Elements can be added to the queue using the `queue.push_front` and `queue.push_back` functions:

```gleam
let one_to_four = queue.from_list([1, 2, 3, 4])
let one_to_five = queue.push_back(one_to_four, 5)
let zero_to_five = queue.push_front(one_to_five, 0)
```

Elements can be removed from the queue using the `queue.pop_front` and `queue.pop_back` functions:

```gleam
let one_to_four = queue.from_list([1, 2, 3, 4])

queue.pop_back(one_to_four)
// -> Ok(#(4, queue.from_list([1, 2, 3])))

queue.pop_front(one_to_four)
// -> Ok(#(1, queue.from_list([2, 3, 4])))

queue.pop_front(queue.new())
// -> Error(Nil)
```

A queue can be converted into a list using the `queue.to_list` function:

```gleam
let one_to_four = queue.from_list([1, 2, 3, 4])
queue.to_list(one_to_four)
// -> [1, 2, 3, 4]
```

### Queue equality

Due to how queues are implemented, two queues with the same elements in the same order may not be equal according to the `==` operator, which compares values structurally. For example:

```gleam
let empty = queue.new()
let a = queue.push_front(empty, 1)
let b = queue.push_back(empty, 1)
a == b
// -> False
```

If you need to compare two queues for equality you can use the `queue.is_logically_equal` function.

```gleam
queue.is_logically_equal(a, b, fn(x, y) { x == y })))
// -> True
```

## Instructions

As a magician-to-be, Elyse needs to practice some basics. She has a stack of cards that she wants to manipulate.

To make things a bit easier she only uses the cards 1 to 10.

The stack of cards is represented by a `queue`, with the bottom of the stack at the front of the queue, and the top of the stack at the back.

## 1. Insert a card at the of top the stack

Implement the function `insert_top` that returns a copy of the stack with the new card provided added to the top of the stack.

```gleam
insert_top(queue.from_list([5, 9, 7, 1]), 8)
// -> queue.from_list([5, 9, 7, 1, 8])
```

## 2. Remove the top card from the stack

Implement the function `remove_top_card` that returns a copy of the stack having had its top card removed. If the given stack is empty, the original stack should be returned, unchanged.

```gleam
remove_top_card(queue.from_list([3, 2, 6, 4, 8]))
// -> queue.from_list([3, 2, 6, 4])
```

## 3. Insert a card at the bottom of the stack

Implement the function `insert_bottom` that returns a copy of the stack with the new card provided added to the bottom of the stack.

```gleam
insert_bottom(queue.from_list([5, 9, 7, 1]), 8)
// -> queue.from_list([8, 5, 9, 7, 1])
```

## 4. Remove a card from the bottom of the stack

Implement the function `remove_bottom_card` that returns a copy of the stack having had its bottom card removed. If the given stack is empty, the original stack should be returned, unchanged.

```gleam
remove_bottom_card(queue.from_list([8, 5, 9, 7, 1]))
// -> queue.from_list([5, 9, 7, 1])
```

## 5. Check size of the stack

Implement the function `check_size_of_stack` that checks whether the size of the stack is equal to a given `stack_size` or not.

```gleam
check_size_of_stack(queue.from_list([3, 2, 6, 4, 8]), 4)
// -> False
```

## Source

### Created by

- @lpil