# My F# Journey


## Exercises I've been mentored on

- [Log Levels](https://exercism.org/tracks/fsharp/exercises/log-levels/mentor_discussions/89347e5718624ff7bbf8db328a9e3419)
- [Space Age](https://exercism.org/tracks/fsharp/exercises/space-age/mentor_discussions/be6acd0da78447f183837d915bcb40e1)

### Awaiting

- [Triangle](https://exercism.org/tracks/fsharp/exercises/triangle/mentor_request)


## An experiment

How to capture an operator as a function?

https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading#overloaded-operator-names

```fsharp
let compare (operand1: int) (cmp: int -> int -> bool) (operand2: int): bool =
    cmp operand1 operand2

let res = compare 4 op_Equality 6

printfn  $"{res}"       // False
```

## F# scripting

https://learn.microsoft.com/en-us/dotnet/fsharp/tools/fsharp-interactive/ 
