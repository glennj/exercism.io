# About

## Comparing Numbers

In `jq` numbers can be compared using the following relational and equality operators.

| Comparison             | Operator |
| ---------------------- | -------- |
| Greater than           | `a > b`  |
| Greater than or equals | `a >= b` |
| Less than              | `a < b`  |
| Less than or equals    | `a <= b` |
| Equals                 | `a == b` |
| Not equals             | `a != b` |

The result of the comparison is always a boolean value, so either `true` or `false`.

```jq
1 < 3,      # => true
2 != 2,     # => false
1 == 1.0    # => true
            # All numbers are floating-points, so this is different syntax
            # for the exact same value.
```

## Comparing Strings

The comparison operators above can also be used to compare strings.
In that case, a dictionary (lexicographical) order is applied.
The ordering is [by unicode codepoint value][sort].

[sort]: https://jqlang.github.io/jq/manual/v1.7/#sort-sort_by

```jq
"Apple" > "Pear",   # => false
"a" < "above",      # => true
"a" == "A"          # => false
```

You need to be careful when you compare two variables that appear to contain numeric values but are of type string.
Due to the dictionary order, the result will not be the same as comparing values of type number.

```jq
10 < 2,     # => false
"10" < "2"  # => true (because "1" comes before "2")
```

## "Strict" Equality

The `jq` `==` operator is like Javascript's `===` in the sense that things that "look" the same, but are of different types, are not equal.

```jq
"3" == 3    # => false
            # the value on the left has type string,
            # the value on the right has type number.
```

## Comparing Arrays

Two arrays are equal if all the corresponding elements are equal.

```jq
[1, 2, 3] == [1, 2, 3]      # => true
[1, 2, 3] == [1, 3, 2]      # => false, different order
[1, 2, 3] == [1, 2, "3"]    # => false, different types
```

## Comparing Objects

Two objects are equal if they have the same key-value pairs.

```jq
{name: "Joe", age: 42} == {age: 42, name: "Joe"}                # => true
{name: "Joe", age: 42} == {age: 42, name: "Jane"}               # => false
{name: "Joe", age: 42} == {age: "42", name: "Joe"}              # => false
{name: "Joe", age: 42} == {age: 42, name: "Joe", height: 175}   # => false

# comparisons will drill down as deeply as required
{a: {b: {c: [1, 2]}}} == {a: {b: {c: [1, 2]}}}                  # => true
{a: {b: {c: [1, 2]}}} == {a: {b: {c: [1, 2, 3]}}}               # => false
```
