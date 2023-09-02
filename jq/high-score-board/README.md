# High Score Board

Welcome to High Score Board on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Objects

A JSON **object** is, to use terminology from other languages, a "hash", "map", or "dictionary".

JSON defines an _object_ as:

> An object is an unordered set of **name**/**value** pairs.
> An object begins with `{` left brace and ends with `}` right brace.
> Each _name_ is followed by `:` colon and the _name/value_ pairs are separated by `,` comma.

The _name_ **must be a string**.
Another word for _name_ is _key_.

The _value_ can be of any JSON type.
Different _values_ in the same object can be of different types, like this example.

<!-- prettier-ignore -->
```json
{
  "name": "Jane",
  "age": 42,
  "pets": ["cat", "fish"],
  "address": {"street": "123 Main St", "city": "Springfield"}
}
```

<!-- prettier-ignore-end -->

Note that there **must not** be a comma following the _last_ key-value pair.

### Creating objects

Use braces to collect the name/value pairs.
Even though the names must be strings, they do not need to be quoted if the names are **identifier-like** (composed of alphanumeric characters and underscore, and not started with a digit).

```jq
{name: "Jane", age: 42}
```

It is valid to use keys that are not _identifier-like_.
Just quote them.

```jq
{"first name": "Jane", "last name": "Lopez", age: 42}
```

If the name is the result of an expression, the expression **must** be in parentheses.

```sh
$ echo "Jane" | jq -Rc '{.: 42}'
# verbose error message ...

$ echo "Jane" | jq -Rc '{(.): 42}'
{"Jane":42}
```

### Indexing

Values are retrieved from an object with **dot notation**.

```jq
{name: "Jane", age: 42} | .age    # => 42
```

If you cannot refer to the key as an identifier, use **bracket notation**.

```jq
"name" as $key | {name: "Jane", age: 42} | .$key    # => error
"name" as $key | {name: "Jane", age: 42} | .[$key]  # => "Jane"
```

### Adding or changing key-value pairs

To add a new key-value pair to an array, or to update the value of an existing key, use the `=` assignment operator, with an index expression on the left-hand side.

```jq
{name: "Jane", age: 42} | .sport = "tennis" | .age = 21
# => {
#      "name": "Jane",
#      "age": 21,
#      "sport": "tennis"
#    }
```

The `+` operator will _merge_ objects.

```jq
{Richard: 54} + {Jane: 42}
# => {
#      "Richard": 54,
#      "Jane": 42
#    }
```

### Removing a key

Use the `del` function to remove a key.
It returns the updated object.

```jq
{name: "Jane", age: 42} | del(.age)   # => {"name": "Jane"}
```

The parameter to `del` is an **index expression** (using dot- or bracket-notation) that resolves to a key in the object.
`jq` calls it a **path expression**.
It is not sufficient to just give a string.

```jq
{name: "Jane", age: 42} | del(name)        # error: name/0 is not defined
{name: "Jane", age: 42} | del("name")      # error: Invalid path expression with result "name"
{name: "Jane", age: 42} | del(.name)       # OK
{name: "Jane", age: 42} | del(.["name"])   # OK
```

### Membership

- To test if the object has a key, use the `has` function.

  ```jq
  {name: "Jane", age: 42} as $example
  |
    ($example | has("name")),    # => true
    ($example | has("sport"))    # => false
  ```

- Test if a key is in an object with `in`.

  ```jq
  {name: "Jane", age: 42} as $example
  |
    ("name"  | in($example)),    # => true
    ("sport" | in($example))     # => false
  ```

### List all the keys

Use the `keys` function to output a list of all the keys.

```jq
{name: "Jane", age: 42} | keys   # => ["age", "name"]
```

Note that `keys` will _sort_ the keys.
To retrieve the keys in the original order, use `keys_unsorted`.

There is no equivalent function to list all the _values_.
However the `.[]` filter outputs the object values as a _stream_, and that stream can be captured with the `[...]` array constructor.

```jq
[{first: "Jane", last: "Lopez", status: "awesome!"} | .[]]
# => ["Jane", "Lopez", "awesome!"]
```

### Iterating

- The `map_values(filter)` function applies the filter to each _value_ in the object.

  ```jq
  {first: "Jane", last: "Lopez", status: "awesome!"}
  | map_values(ascii_upcase)
  # => {"first": "JANE", "last": "LOPEZ", "status": "AWESOME!"}
  ```

- To iterate over an object, we must first convert it to an array of key-value objects.
  The `to_entries` function does that.

  ```jq
  {name: "Jane", age: 42} | to_entries'
  # => [
  #      {
  #        "key": "name",
  #        "value": "Jane"
  #      },
  #      {
  #        "key": "age",
  #        "value": 42
  #      }
  #    ]
  ```

  At this point, we can use array iteration functions, like `map`.

- The `from_entries` function is the inverse: convert an array of key-value objects into an object.

  ```jq
  [
    {"key":"name", "value":"Jane"},
    {"key":"age", "value":42}
  ] | from_entries                  # =>{"name": "Jane", "age": 42}
  ```

- To apply a filter to _each_ key-value pair in an object, use the `with_entries(filter)` function.

  For example, given an object that maps a name to an age, the keys and values can be swapped as follows.

  ```jq
  {"Jane": 42, "Richard": 54}
  | with_entries({key: (.value | tostring), value: .key})
  ```

  outputs

  ```json
  {
    "42": "Jane",
    "54": "Richard"
  }
  ```

  `with_entries(filter)` is the same as

  ```jq
  to_entries | map(filter) | from_entries
  ```

## Instructions

In this exercise, you are implementing a way to keep track of the high scores for the most popular game in your local arcade hall.

You have 6 functions to implement, mostly related to manipulating an object that holds high scores.

## 1. Create a new high score board

Write a function `create_score_board` which creates an object that serves as a high score board.
The keys of this object will be the names of the players, the values will be their scores.
For testing purposes, you want to directly include one entry in the object.
This initial entry should consist of `"The Best Ever"` as player name and `1000000` as score.

```jq
create_score_board
# returns an object with one initial entry
```

## 2. Add players to a score board

To add a player to the high score board, implement the function `add_player`.
It takes a score board as input, and needs two parameters: the player name and the player's score.
The function outputs the score board object with the new player added.

```jq
{"José Valim", 486373}
| add_player("Dave Thomas"; 0)
# => {"Dave Thomas": 0, "José Valim": 486373} -- in some order
```

## 3. Remove players from a score board

If players violate the rules of the arcade hall, they are manually removed from the high score board.
Implement `remove_player` which takes a board as input and one parameter, the name of the player to remove.
This function should remove the entry for the given player from the board and output the new board.
If the player was not on the board in the first place, nothing should happen to the board; it should be returned as is.

```q
{"Dave Thomas": 0} | remove_player("Dave Thomas")
# => {}

{"Dave Thomas": 0} | remove_player("Rose Fanaras")
# => {"Dave Thomas": 0}
```

## 4. Increase a player's score

If a player finishes another game at the arcade hall, a certain amount of points will be added to the previous score on the board.
Implement `update_score`, which takes a score board as input, and needs two parameters: the player name and the amount of score to add.
The function should return the score board after the update was done.

```jq
{"Freyja Ćirić": 12771000} | update_score("Freyja Ćirić"; 73)
# => {"Freyja Ćirić": 12771073}
```

## 5. Apply Monday bonus points

The arcade hall keeps a separate score board on Mondays.
At the end of the day, each player on that board gets 100 additional points.

Implement the function `apply_monday_bonus`.
The function adds the bonus points for each player that is listed on that board.

```jq
{
  "Dave Thomas": 44,
  "Freyja Ćirić": 539,
  "José Valim": 265
}
| apply_monday_bonus
# => {"Dave Thomas": 144, "Freyja Ćirić": 639, "José Valim": 365}
```

## 6. Find the total score

Different arcade halls compete with each other to determine who has the best players.
The arcade with the highest total score wins the honor.

Write a function `total_score`.
It takes a score board as input, and outputs the sum of all the players' scores.

```jq
{
  "Dave Thomas": 44,
  "Freyja Ćirić": 539,
  "José Valim": 265
}
| total_score
# => 848
```

## Source

### Created by

- @glennj