# Piper's Pie

Welcome to Piper's Pie on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Tail Call Recursion

A function is [tail-recursive][recursion-tc] if the _last_ thing executed by the function is a call to itself.

Each time any function is called, a _stack frame_ with its local variables and arguments is put on top of [the function call stack][call-stack].
When a function returns, the stack frame is removed from the stack.

Tail-recursive functions allow for [tail call optimization][tail-call-optimization] (or tail call elimination).
It's an optimization that allows reusing the last stack frame by the next function call when the previous function is guaranteed not to need it anymore.
This mitigates concerns of overflowing the function call stack, which is a situation when there are so many frames on the function call stack that there isn't any memory left to create another one.

### Tail Call Optimization in Elm

Under some condition, the Elm compiler is able to automatically performs an a tail call optimization when compiling to JavaScript.

The optimization can happen for a recursive function when the _last_ operation in a branch is done by calling the function itself in a simple function application.
Let's look at some examples:

```elm
factorial : Int -> Int
factorial n =
  if n <= 1 then
    n
  else
    n * factorial (n-1)
```

The implementation above is not tail recursive, because the last operation in the `else` branch is a multiplication `n *`.

```elm
factorial : Int -> Int
factorial n =
  factorialHelper n n

factorialHelper : Int -> Int -> Int
factorialHelper n resultSoFar =
  if n <= 1 then
    resultSoFar
  else
    factorialHelper (n-1) (n * resultSoFar)
```

The implementation above is tail recursive and will be optimized, because the last operation in the `else` branch is `factorialHelper` calling itself.
This would not be possible for a function with the type signature `Int -> Int`, and in practice tail call optimization is often achieved by defining helper functions.

[recursion-tc]: https://en.wikipedia.org/wiki/Tail_call
[call-stack]: https://en.wikipedia.org/wiki/Call_stack

## Instructions

Piper is an avid pie baker.

No one knows if she picked pie baking because of her name, or if she changed her name to match her hobby.
At a glance, the latter doesn't seem very likely, but you see, Piper is absolutely fascinated by pies.
She's always tinkering in the kitchen, tweaking her recipes, improving her craft, to the absolute delight of her friends.

Her latest interest?
Baking pies as circular as possible, to the point of mathematical perfection, with the help of her favorite number, you guessed it: π.

Piper found a delightful formula to calculate π iteratively, the Newton/Euler Convergence Transformation:

```
π / 2 = sum for k from 0 to infinity of ( k! ) / ( 2 * k + 1 )!!
```

Help Piper bake her mathematically perfect pie by calculating π.

## 1. Factorial

Let's warm up first.
The factorial operator, usually written `!`, is defined as

```
0! = 1
n! = 1 * 2 * 3 * ... * n
```

Define the `factorial` function which will compute the factorial in a tail recursive manner.

```elm
factorial 4
    -- 24
```

## 2. Double Factorial

The double factorial operator, usually written `!!`, is defined as

```
0!! = 1
n!! = 1 * 3 * 5 * ... * n (for odd n)
n!! = 2 * 4 * 6 * ... * n (for even n)
```

Define the `doubleFactorial` function which will compute the factorial in a tail recursive manner.

```elm
factorial 5
    -- 15
factorial 6
    -- 48
```

## 3. Newton/Euler Convergence Transformation

Define the `pipersPi` function, which will approximate π using a set number of terms from the Newton/Euler Convergence Transformation formula in a tail recursive manner.

```
π / 2 = sum for k from 0 to infinity of ( k! ) / ( 2 * k + 1 )!!
```

Let's compute the first term together.
For an upper limit of `0` (instead of infinity), we get:

```
π / 2 ≈ Sum for k from 0 to 0 of ( k! ) / ( 2 * k + 1 )!!
π / 2 ≈ ( 0! ) / ( 2 * 0 + 1 )!!
π / 2 ≈ 0! / 0!!
π / 2 ≈ 1 / 1
π / 2 ≈ 1
π ≈ 2
```

Each extra term will improve the approximation.

```elm
pipersPi 0
    -- 2.0
pipersPi 1
    -- 2.6666666
```

## Source

### Created by

- @jiegillet