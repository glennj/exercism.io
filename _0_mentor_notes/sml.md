# Standard ML

## Mutually Recursive

Datatypes and functions can be mutually recursive

```sml
fun even 0 = true
  | even n = odd (n - 1)
and odd 0 = false
  | odd n = even (n - 1)
```

Since an indentifier needs to be declared before it is used,
such a mutually recursive definition would be impossible
with the keyword `and`.

A datatype example

```sml
datatype a = A | Ab of b
and      b = B | Ba of a
```

