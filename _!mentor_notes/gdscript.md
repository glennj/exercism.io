# Notes about GDScript

<!-- #################################################### -->

## Recursive Lambdas

Example: [flatten-array](https://github.com/glennj/exercism.io/blob/main/gdscript/flatten-array/flatten_array.gd)

Recursive _functions_ are fine:

```gdscript
func gcd(a, b):
    if b == 0:
        return a
    return gcd(b, a % b)
```

However if we want to turn that into lambda, it will fail
```gdscript
var gcd = func (a, b):
    if b == 0:
        return a
    return gcd(b, a % b)
```
At the moment of definition, `gcd` is null, so the recursive call fails.

What if we forward declare it? Same failure, the value is null at that moment
```gdscript
var gcd: Callable
gcd = func (a, b):
    if b == 0:
        return a
    return gcd(b, a % b)
```

We have to pass the lambda as a parameter to itself:
```gdscript
var gcd = func (lambda, a, b):
    if b == 0:
        return a
    return lambda(lambda, b, a % b)

var g = gcd.call(gcd, 51, 17)
```

<!-- #################################################### -->

## Closures

Example: [camicia](https://github.com/glennj/exercism.io/blob/main/gdscript/camicia/camicia.gd#L21)

Similar to the above, _altering_ an outer-scope variable inside a lambda only alters the variable inside the lambda: the changed value is lost when the lambda ends:

```gdscript
var x = 1

var incr_x = func():
    x += 1
    print("in function, after incrementing, x = %d" % x)

incr_x.call()
print("after function, x = %d" % x)
```
will output

```none
in function, after incrementing, x = 2
after function, x = 1
```

Any value you want to alter in a lambda must be contained in a reference type

```gdscript
var state = {x = 1}

var incr_x = func():
    state.x += 1
    print("in function, after incrementing, x = %d" % state.x)

incr_x.call()
print("after function, x = %d" % state.x)
```

<!-- #################################################### -->

## Iterator Protocol

AKA "custom iterators"

Example: [simple-linked-list](https://github.com/glennj/exercism.io/blob/f9b8528c629b91c933653606057b0029fdc09149/gdscript/simple-linked-list/simple_linked_list.gd#L16)

Ref [Custom Iterators](https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/gdscript_advanced.html#custom-iterators
) in the tutorial.
