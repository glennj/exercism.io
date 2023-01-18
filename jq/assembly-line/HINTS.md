# Hints

## General

- Inside a function, use the identity expression `.` to access the input.
- The output of a function is the result of the last expression in the function's pipeline.

## 1. Calculate the production rate per hour

- This is the assembly line's speed multiplied by the base cars per hour multiplied by the success rate.
- Determining the success rate can be done through a [conditional statement][if-then-else].
- Multiplication uses the `*` operator.
- Numbers can be compared using the built-in [equality][eq] and [comparison][cmp] operators.

## 2. Calculate the number of working items produced per minute

- This is the production rate per hour divided by the number of minutes per hour, truncated to an integer.
- Division uses the `/` operator.
- The result of the division will need to be truncated with [the `trunc` function][math-funcs].

[eq]: https://stedolan.github.io/jq/manual/v1.6/#==,!=
[cmp]: https://stedolan.github.io/jq/manual/v1.6/#%3E,%3E=,%3C=,%3C
[if-then-else]: https://stedolan.github.io/jq/manual/v1.6/#if-then-else
[math-funcs]: https://stedolan.github.io/jq/manual/v1.6/#Math