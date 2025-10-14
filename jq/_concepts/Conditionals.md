# About

## If Expression

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
Omitting it produces errors.

```sh
$ /bin/jq --version
jq-1.6
$ /bin/jq -n 'if 1 < 2 then "OK" end'
jq: error: syntax error, unexpected end (Unix shell quoting issues?) at <top-level>, line 1:
if 1 < 2 then "OK" end
jq: error: Possibly unterminated 'if' statement at <top-level>, line 1:
if 1 < 2 then "OK" end
jq: 2 compile errors
```
~~~~

## Nested If-Statements

Further conditions can be added with `elif`.

```jq
42 | if . < 33 then "small"
     elif . < 66 then "medium"
     else "big"
     end
# => "medium"
```

Use as many `elif` clauses as you need.

## Truthiness

The only "false" values in `jq` are: `false` and `null`.
Everything else is "true", even the number zero and the empty string, array and object.

## Boolean Operators

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

## Alternative Operator

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

## Are there more?

Programmers coming from different languages might be expecting a wider variety of conditional commands, such as `cond`, `case`, `switch`, `when`, `unless`, a ternary operator, etc.
The operators listed above are everything that `jq` offers.
