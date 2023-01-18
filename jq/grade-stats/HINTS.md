# Hints

## Translate a numeric grade to a letter grade

- Use an [`if-then-else` expression][jq-man-if] with as many `elif` branches as needed.

## Count the number of students for each letter grade

- The `reduce` filter needs a _stream_ to iterate over.
  The input is an object.
  Use the `to_entries` expression to transform the object into an array.
- The result object must have all the possible letter grades as keys, even if no students earned that grade.
- The [`+=` assignment][jq-man-arith-assign] will be useful.

[jq-man-if]: https://stedolan.github.io/jq/manual/v1.6/#if-then-else
[jq-man-arith-assign]: https://stedolan.github.io/jq/manual/v1.6/#Arithmeticupdate-assignment:+=,-=,*=,/=,%=,//=