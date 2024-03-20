# Vehicle Purchase

Welcome to Vehicle Purchase on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Compare

### Comparing Numbers

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

### Comparing Strings

The comparison operators above can also be used to compare strings.
In that case, a dictionary (lexicographical) order is applied.
The ordering is _by unicode codepoint value_.

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

### "Strict" Equality

The `jq` `==` operator is like Javascript's `===` in the sense that things that "look" the same, but are of different types, are not equal.

```jq
"3" == 3    # => false
            # the value on the left has type string,
            # the value on the right has type number.
```

### Comparing Arrays

Two arrays are equal if all the corresponding elements are equal.

```jq
[1, 2, 3] == [1, 2, 3]      # => true
[1, 2, 3] == [1, 3, 2]      # => false, different order
[1, 2, 3] == [1, 2, "3"]    # => false, different types
```

### Comparing Objects

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

## Conditionals

### If Expression

`jq`'s **conditional expression** is `if A then B else C end`.

`if-then-else` is a filter like all `jq` builtins: it takes an input and produces an output.

If the expression `A` produces a "truthy" value, then the `if` filter evaluates `B`.
Otherwise it evaluates `C`.
The input to the `if` filter will be passed to `B` or `C`.

```jq
42 | if . < 50 then "small" else "big" end      # => "small"
```

```jq
5 | if . % 2 == 0 then . / 2 else . * 4 end     # => 20
```

~~~~exercism/note
The `else` clause is **optional** in the current `jq` release (version 1.7):
the following two statements are equivalent.

```jq
if A then B else . end
if A then B end
```

The `else` clause is **mandatory** in the previous v1.6 release. 
~~~~

### Nested If-Statements

Further conditions can be added with `elif`.

```jq
42 | if . < 33 then "small"
     elif . < 66 then "medium"
     else "big"
     end
# => "medium"
```

Use as many `elif` clauses as you need.

### Truthiness

The only "false" values in `jq` are: `false` and `null`.
Everything else is "true", even the number zero and the empty string, array and object.

### Boolean Operators

The **boolean operators** `and` and `or` can be used to build complex queries.

```jq
42 | if . < 33 or . > 66 then "big or small"
     else "medium"
     end
```

To negate, use `not`. This is a **filter** not an operator.

```jq
42 | if (. < 33 or . > 66 | not) then "medium"
     else "big or small"
     end
```

### Alternative Operator

The **alternative operator** allows you to specify a "default" value if an expression is false or null.

```jq
A // B

# This is identical to
if A then A else B end
```

To demonstrate

```jq
[3, 5, 18] | add / 2       # => 13
[]         | add / 2       # => error: null (null) and number (2) cannot be divided
[]         | add // 0 / 2  # => 0
```

## Instructions

In this exercise, you will write some code to help you prepare to buy a vehicle.

You have three tasks: determine if you will need to get a license; choose between two vehicles; and estimate the acceptable price for a used vehicle.

## 1. Determine if you will need a drivers license

Some kinds of vehicles require a drivers license to operate them.
Assume only the kinds `"car"` and `"truck"` require a license; everything else can be operated without a license.

Implement the `needs_license` function that takes the kind of vehicle and returns a boolean indicating whether you need a license for that kind of vehicle.

```jq
"car" | needs_license
# => true

"bike" | needs_license
# => false
```

## 2. Choose between two potential vehicles to buy

You have evaluated your options of available vehicles.
You managed to narrow it down to two options but you need help making the final decision.
Implement the function `choose_vehicle` that takes an array of two vehicles as input and returns a decision, which is the option that comes first in dictionary order.

```jq
["Wuling Hongguang", "Toyota Corolla"] | choose_vehicle
# =>  "Toyota Corolla is clearly the better choice."

["Volkswagen Beetle", "Volkswagen Golf"] | choose_vehicle
# =>  "Volkswagen Beetle is clearly the better choice."
```

## 3. Calculate an estimation for the price of a used vehicle

Now that you have made your decision, you want to make sure you get a fair price at the dealership.
Since you are interested in buying a used vehicle, the price depends on how old the vehicle is.
For a rough estimate, assume if the vehicle is less than 3 years old, it costs 80% of the original price it had when it was brand new.
If it is more than 10 years old, it costs 50%.
If the vehicle is at least 3 years old but not older than 10 years, it costs 70% of the original price.

Implement the `resell_price` function that applies this logic using `if`, `elif` and `else`.
It takes an object holding the original price and the age of the vehicle and returns the estimated price in the dealership.

```jq
{"original_price": 1000, "age": 1} | resell_price
# => 800

{"original_price": 1000, "age": 5} | resell_price
# => 700

{"original_price": 1000, "age": 15} | resell_price
# => 500
```

## Source

### Created by

- @glennj