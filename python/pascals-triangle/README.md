# Pascal's Triangle

Welcome to Pascal's Triangle on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

With the weather being great, you're not looking forward to spending an hour in a classroom.
Annoyed, you enter the class room, where you notice a strangely satisfying triangle shape on the blackboard.
Whilst waiting for your math teacher to arrive, you can't help but notice some patterns in the triangle: the outer values are all ones, each subsequent row has one more value than its previous row and the triangle is symmetrical.
Weird!

Not long after you sit down, your teacher enters the room and explains that this triangle is the famous [Pascal's triangle][wikipedia].

Over the next hour, your teacher reveals some amazing things hidden in this triangle:

- It can be used to compute how many ways you can pick K elements from N values.
- It contains the Fibonacci sequence.
- If you color odd and even numbers differently, you get a beautiful pattern called the [Sierpiński triangle][wikipedia-sierpinski-triangle].

The teacher implores you and your classmates to look up other uses, and assures you that there are lots more!
At that moment, the school bell rings.
You realize that for the past hour, you were completely absorbed in learning about Pascal's triangle.
You quickly grab your laptop from your bag and go outside, ready to enjoy both the sunshine _and_ the wonders of Pascal's triangle.

[wikipedia]: https://en.wikipedia.org/wiki/Pascal%27s_triangle
[wikipedia-sierpinski-triangle]: https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle

## Instructions

Your task is to output the first N rows of Pascal's triangle.

[Pascal's triangle][wikipedia] is a triangular array of positive integers.

In Pascal's triangle, the number of values in a row is equal to its row number (which starts at one).
Therefore, the first row has one value, the second row has two values, and so on.

The first (topmost) row has a single value: `1`.
Subsequent rows' values are computed by adding the numbers directly to the right and left of the current position in the previous row.

If the previous row does _not_ have a value to the left or right of the current position (which only happens for the leftmost and rightmost positions), treat that position's value as zero (effectively "ignoring" it in the summation).

## Example

Let's look at the first 5 rows of Pascal's Triangle:

```text
    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1
```

The topmost row has one value, which is `1`.

The leftmost and rightmost values have only one preceding position to consider, which is the position to its right respectively to its left.
With the topmost value being `1`, it follows from this that all the leftmost and rightmost values are also `1`.

The other values all have two positions to consider.
For example, the fifth row's (`1 4 6 4 1`) middle value is `6`, as the values to its left and right in the preceding row are `3` and `3`:

[wikipedia]: https://en.wikipedia.org/wiki/Pascal%27s_triangle

## How this Exercise is Implemented in Python: Recursion

This exercise is designed to be completed using [concept:python/recursion](), rather than loops.
A recursive function is a function that calls itself, which is useful when solving problems that are defined in terms of themselves.
To avoid infinite recursion (or more specifically, to avoid overflowing the stack), something called a "base case" is used.
When the base case is reached, a non-recursive value is returned, which allows the previous function call to resolve and return its value, and so on, rippling back down the stack until the first function call returns the answer.
We could write a recursive function to find the answer to 5! (i.e. 5 * 4 * 3 * 2 * 1) like so:

````python
def factorial(number):
  if number <= 1:  # base case
    return 1

  return number * factorial(number - 1) # recursive case

print(factorial(5)) # returns 120
````

Finally, it should be noted that Python limits the number of times recursive calls can be made (1000 by default) and does not optimize for [tail recursion][tail-recursion].

## Exception messages

Sometimes it is necessary to [raise an exception][raising].
When you do this, you should always include a **meaningful error message** to indicate what the source of the error is.
This makes your code more readable and helps significantly with debugging.
For situations where you know that the error source will be a certain type, you can choose to raise one of the [built in error types][built-in-errors], but should still include a meaningful message.

This particular exercise requires that you use the [raise statement][raise-statement] to "throw" multiple `ValueErrors` if the `rows()` function is passed a negative number.
The tests will only pass if you both `raise` the `exception` and include a message with it.

To raise a `ValueError` with a message, write the message as an argument to the `exception` type:

```python
# if the rows function is passed a negative number.
raise ValueError("number of rows is negative")
```

[built-in-errors]: https://docs.python.org/3/library/exceptions.html#base-classes
[raise-statement]: https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement
[raising]: https://docs.python.org/3/tutorial/errors.html#raising-exceptions
[tail-recursion]: https://www.geeksforgeeks.org/tail-recursion/

## Source

### Created by

- @betegelse
- @PaulT89

### Contributed to by

- @behrtam
- @cmccandless
- @Dog
- @ikhadykin
- @ikkebr
- @kytrinyx
- @N-Parsons
- @olufotebig
- @parinporecha
- @pheanex
- @sjakobi
- @tqa236

### Based on

Pascal's Triangle at Wolfram Math World - https://www.wolframalpha.com/input/?i=Pascal%27s+triangle