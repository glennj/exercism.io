# Hints

## 1. Create a new high score board

- Create a new object using curly braces.
- Write the key as a string so the key can contain spaces.
- Separate key and value using a colon.

## 2. Add players to a score board

- Use bracket notation to add a key with a name that is stored in a variable (the argument).
- Use the assignment operator (`=`) to set a value for the new key.
- Alternately, use the [`+` operator][man-plus] to merge the incoming object with a new one.
- Remember that parentheses are needed around expressions for object keys.

## 3. Remove players from a score board

- Use the [del expression][man-del].
- Reference the key like you have done in the task before (bracket notation).
- Remember that the argument to `del` is a _index expression_ not just a string.

## 4. Increase a player's score

- First think about how to express the new value that you want to assign.
- Then use the assignment operator like in task 2 to set that new value.
- If you have not done so already, you can make use of the [arithmetic update-assignment operator][man-update-assignment] `+=`.

## 5. Apply Monday bonus points

- This would be a good place to use `to_entries`/`from_entries`, or `with_entries` to iterate over the object
- For each key, set the new value as you did in task 4.

## 6. Find the total score

- We want to iterate over the _values_ of the object.
  We can use [`map_values`][man-map_values], or [`.[]`][man-brackets] to output a stream of values.
- The `add` expression can be used to find the sum of a list of numbers.

[man-plus]: https://jqlang.github.io/jq/manual/v1.6/#Addition:+
[man-del]: https://jqlang.github.io/jq/manual/v1.6/#del(path_expression)
[man-update-assignment]: https://jqlang.github.io/jq/manual/v1.6/#Arithmeticupdate-assignment:+=,-=,*=,/=,%=,//=
[man-map_values]: https://jqlang.github.io/jq/manual/v1.6/#map(x),map_values(x)

[man-brackets]: https://jqlang.github.io/jq/manual/v1.6/#Array/ObjectValueIterator:.[]