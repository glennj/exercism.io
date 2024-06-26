# Hints

## General

- Try to split a problem into a base case and a recursive case
  For example, let's say you want to count how many cookies are there in the cookie jar with a recursive approach
  A base case is an empty jar - it has zero cookies.
  If the jar is not empty, then the number of cookies in the jar is equal to one cookie plus the number of cookies in the jar after removing one cookie.

## 1. Implement a function to add the numbers in an array

- The recursion is to iterate over elements in the array, adding each to an accumulator.
- The base case is an empty array, which has sum equal to zero.
- The recursive case adds the first element to the sum of the rest of the array.
- Recall array indexing and slicing from [the Arrays concept][arrays-concept] to separate the first array element from the rest of the elements.

## 2. Reverse an array

- The recursion is to iterate over elements in the array, adding each to another array in reverse order.
- The base case is an empty array.
- Remember that the [`+` operator][manual-addition] concatenates when applied to arrays.

## 3. Map an array

- The recursion is to iterate over elements in the array, adding to an accumulator array the result of applying the filter to the element.
- To apply the filter parameter to an element, use the usual pipe syntax: `.element | filter`.
- The base case is an empty array.

[arrays-concept]: /tracks/jq/concepts/arrays
[manual-addition]: https://jqlang.github.io/jq/manual/v1.7/#addition