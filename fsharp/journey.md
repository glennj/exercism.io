# My F# Journey


## Exercises I've been mentored on

- [Log Levels](https://exercism.org/tracks/fsharp/exercises/log-levels/mentor_discussions/89347e5718624ff7bbf8db328a9e3419)
- [Space Age](https://exercism.org/tracks/fsharp/exercises/space-age/mentor_discussions/be6acd0da78447f183837d915bcb40e1)
- [Meetup](https://exercism.org/tracks/fsharp/exercises/meetup/mentor_discussions/670c40ed35184f0991f969153671824a)
  - illuminating discussion about when/why parens are needed.
- [Triangle](https://exercism.org/tracks/fsharp/exercises/triangle/mentor_request)
- [Grade School](https://exercism.org/tracks/fsharp/exercises/grade-school/mentor_discussions/12e4efde29ba4ad2826a2391d3a5cf28)
- [Bank Account](https://exercism.org/tracks/fsharp/exercises/bank-account/mentor_discussions/f5c6036164b944e3817815a44068e213)
  - references and locks


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
