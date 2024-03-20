# Hints

## 1. Determine if you will need a drivers license

- Use the [equality operator] to check whether your input equals a certain string.
- Use one of the two [boolean operators] to combine the two requirements.
- You do not need an if-statement to solve this task. You can return the boolean expression you build directly.

## 2. Choose between two potential vehicles to buy

- You will need the `if-then-else` [conditional expression] for this task.
- Use a [comparison operator] to determine which option comes first in dictionary order.
- Finally, construct the recommendation sentence.
  You can use the concatenation or string interpolation that you learned in the Strings concept to construct the recommendation sentence.

## 3. Calculate an estimation for the price of a used vehicle

- Start with determining the percentage based on the age of the vehicle.
  Use an `if-elsif-else` expression.
- To calculate the result, apply the percentage to the original price.
  For example, `30% of x` can be calculated by multiplying `x` by `30` and dividing by `100`.

[equality operator]: https://jqlang.github.io/jq/manual/v1.7/#==-!=
[boolean operators]: https://jqlang.github.io/jq/manual/v1.7/#and-or-not
[comparison operator]: https://jqlang.github.io/jq/manual/v1.7/#%3E-%3E=-%3C=-%3C
[conditional expression]: https://jqlang.github.io/jq/manual/v1.7/#if-then-else-end